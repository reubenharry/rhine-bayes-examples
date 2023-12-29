module Tutorial where

import Concurrent
import Prelude hiding ((.))
import Control.Monad.Bayes.Class (MonadDistribution (normal))
import Example (Result (Result, latent, measured, particles), decayingIntegral, renderObjects, empirical)
import FRP.Rhine.Gloss
import Inference (params, particleFilter, SMCSettings (n))
import Linear (V2 (..))
import Numeric.Log (Log)
import Util
import Control.Lens
import Data.Generics.Product (the)
import Witch (into)
import Convention (AgentID, AgentNumber (One, Two), SAgentNumber (SOne, STwo), one, two, AgentAction(..), Other, other)
import Control.Category ((.))

-----------------------
-- first example
-----------------------



demo1 :: UserInput >--> Picture
demo1 = proc _ -> do

    latent <- worldModel -< ()
    observations <- observationModel -< latent
    beliefAboutState <- inferredPosterior -< observations

    picture <- renderObjects -< Result
      {measured = observations,
      latent = latent,
      particles = beliefAboutState}

    returnA -< picture


type Observation = V2 Double
type State = V2 Double

whiteNoise :: () >--> Double
whiteNoise = constM (normal 0 1)

particlePosition1D :: () >--> Double
particlePosition1D = decayingIntegral 1 . decayingIntegral 1 . whiteNoise

worldModel :: () >--> State
worldModel = proc _ -> do
  xPos <- particlePosition1D -< ()
  yPos <- particlePosition1D -< ()
  returnA -< (V2 xPos yPos)

observationModel :: State >--> Observation
observationModel = proc latent -> do
  xNoise <- whiteNoise -< ()
  yNoise <- whiteNoise -< ()
  returnA -< latent + V2 xNoise yNoise



-- (Time -> Observation) -> UnnormalizedDistribution (Time -> State)
-- posteriorDistribution :: MonadMeasure m => MonadicSystem m Observation State
posteriorDistribution :: Observation >-/-> State
-- posteriorDistribution :: Observation -> SamplerIO [((State, MonadicSystem ...), Log Double)]
posteriorDistribution = proc obs -> do
  latent <- worldModel -< () -- latent :: (Time -> State)
  observe -< (normalPdf2D latent 1 obs)
  returnA -< latent

inferredPosterior :: Observation >--> [(State, Log Double)]
-- inferredPosterior :: (Time -> Observation) -> (Time -> [(State, Log Double)])
inferredPosterior = particleFilter params posteriorDistribution










-----------------------
-- second example
-----------------------

worldModel2 :: AgentAction i >--> State
worldModel2 = proc _ -> do
  xPos <- particlePosition1D -< ()
  yPos <- particlePosition1D -< ()
  returnA -< V2 xPos yPos

posteriorDistributionA :: (Observation, AgentAction i) >-/-> State
posteriorDistributionA = proc (obs, action) -> do
  latent <- worldModel2 -< action
  observe -< (normalPdf2D obs 1 latent)
  returnA -< latent

initialAction :: AgentAction i
initialAction = AgentAction Nothing

-- actionModel :: [(State, Log Double)] >--> AgentAction i
actionModel :: a >--> AgentAction i
actionModel = undefined

agent :: AgentID i -> Observation >--> AgentAction i
agent _ = feedback initialAction proc (obs, oldAction) -> do
  belief <- particleFilter params posteriorDistributionA -< (obs, oldAction)
  newAction <- actionModel -< belief
  returnA -< (newAction, newAction)

agent' :: AgentID i -> Observation >--> AgentAction i
agent' _ = constantly empirical . particleFilter params (feedback initialAction proc (obs, oldAction) -> do
  belief <- posteriorDistributionA -< (obs, oldAction)
  newAction <- actionModel -< belief
  returnA -< (newAction, newAction))

demo2' :: System (Stochastic & Feedback) UserInput Picture
demo2' = proc _ -> do
  rec
    observation <- worldModel2 -< action 
    action <- agent one -< observation
  returnA -< mempty


demo2'' :: () >--> (Observation, AgentAction One)
demo2'' = couple (0, AgentAction Nothing) (agent one) worldModel2

couple :: (a, b) -> (a >--> b) -> (b >--> a) -> (() >--> (a, b))
couple initial x y = feedback initial proc ((), (a,b)) -> do
  a' <- y -< b
  b' <- x -< a
  let out = (a',b')
  returnA -< (out, out)

demo2 :: UserInput >--> Picture
demo2 = feedback undefined proc (_, observation) -> do
    action <- agent one -< observation
    newObservation <- worldModel2 -< action
    returnA -< (mempty, newObservation)

-----------------------
-- third example
-----------------------

worldIntegrated :: System (Stochastic & Feedback) (AgentAction i) Observation
worldIntegrated = proc action -> do
  rec
    observation <- worldModel2 -< action 
    action <- agent one -< observation
  returnA -< observation


trueWorldModel :: (AgentAction One, AgentAction Two) >--> State
trueWorldModel = undefined

-- demo3 :: UserInput >-&-> Picture
-- demo3 = proc _ -> do
--   rec
--     observation <- trueWorldModel -< (action1, action2)
--     action1 <- agent one -< observation
--     action2 <- agent two -< observation
--   returnA -< error "todo: visualization"

demo3 :: UserInput >--> Picture
demo3 = feedback (initialAction, initialAction) proc (_, (action1, action2)) -> do
  observation <- trueWorldModel -< (action1, action2)
  newAction1 <- agent one -< observation
  newAction2 <- agent two -< observation
  returnA -< (error "todo: visualization", (newAction1, newAction2))


-----------------------
-- fourth example
-----------------------



demo4 :: System (Stochastic & Feedback) () (Observation, AgentAction i, AgentAction (Other i))
demo4 = proc _ -> do
  rec
    observation <- trueWorldModel -< (action1, action2)
    action1 <- cleverAgent one -< observation
    action2 <- cleverAgent two -< observation
  returnA -< error "todo: visualize"




cleverAgent :: AgentID i -> Observation >--> AgentAction i
cleverAgent agentID = proc obs -> do
  belief <- particleFilter params (cleverPosteriorDistribution agentID) -< obs
  action <- cleverActionModel agentID -< belief
  returnA -< action

  where



    cleverPosteriorDistribution :: AgentID i -> Observation >-/-> State
    cleverPosteriorDistribution agentID = proc obs -> do
      latent <- cleverPriorDistribution agentID -< undefined
      observe -< (normalPdf2D obs 1 latent)
      returnA -< latent

    -- note: this isn't quite right, but for demonstrative purposes...
    cleverPriorDistribution :: forall i. AgentID i -> AgentAction i >--> State
    cleverPriorDistribution agentID = feedback initialAction proc (myAction, otherAgentAction :: AgentAction (Other i)) -> do
        state <- trueWorldModel -< case agentID of
            SOne -> (myAction, otherAgentAction)
            STwo -> (otherAgentAction, myAction)

        obs <- observationModel -< state
        newOtherAgentAction <- agent (other agentID) -< obs

        returnA -< (state, newOtherAgentAction)

    cleverActionModel :: AgentID i -> [(State, Log Double)] >--> AgentAction i
    cleverActionModel agentID = undefined


---- implementation pseudocode

-- type TimeVar = Double

-- -- A 
-- -- B

-- -- A >--> B -- (Time -> A) -> (Time -> B)

-- -- System Double Double

-- data Foo = F Double

-- -- Distribution 

-- -- (a -> Distribution b)
-- -- (b -> Distribution c)

-- -- --->

-- -- a -> Distribution c

-- -- (>>=) :: m a -> (a -> m b) -> m b


-- bar = F 4

-- aa :: Distribution Double
-- aa = normal 0 1 


-- program :: Distribution Double
-- program = do 
--   s <- aa
--   return (s + s)

-- -- 1. SMC in a not reactive context
-- -- 2. definition of feedback
-- -- 3. SMC in a reactive context 


-- newtype SysStep m input output = S (input ->  (TimeVar -> Sampler (output, SysStep m input output)))


-- compose :: Monad m => SysStep m a b -> SysStep m b c -> SysStep m a c
-- compose (S f) (S g) = S (\input time -> do

--   (intermediateOutput, nextF) <- (f input) time
--   (finalOutput, nextG) <- (g intermediateOutput) time
--   return (finalOutput, compose nextF nextG))



-- identity :: SysStep a a
-- identity = S (\input -> (input, identity))





-- main :: SysStep input output -> ([(input, TimeVar)] -> [output])
-- main = undefined