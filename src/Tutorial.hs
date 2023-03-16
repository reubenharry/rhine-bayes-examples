module Tutorial where

import Concurrent
import Control.Monad.Bayes.Class (MonadDistribution (normal))
import Example (Result (Result, latent, measured, particles), decayingIntegral, renderObjects)
import FRP.Rhine.Gloss
import Inference (params, particleFilter, SMCSettings (n))
import Linear (V2 (..))
import Numeric.Log (Log)
import Util
import Control.Lens
import Data.Generics.Product (the)
import Witch (into)
import Convention (AgentID, AgentNumber (One, Two), SAgentNumber (SOne, STwo), one, two, AgentAction, Other, other)

-----------------------
-- first example
-----------------------


demo1 :: System Stochastic UserInput Picture
demo1 = proc _ -> do
  
    latent <- worldModel -< ()
    observations <- observationModel -< latent
    beliefAboutState <- inferredPosterior -< observations

    renderObjects -< Result 
      {measured = observations, 
      latent = latent, 
      particles = beliefAboutState}


type Observation = V2 Double
type State = V2 Double

whiteNoise :: System Stochastic () Double
whiteNoise = constM (normal 0 1)

particlePosition1D :: System Stochastic () Double
particlePosition1D = whiteNoise >>> decayingIntegral 1 >>> decayingIntegral 1

worldModel :: System Stochastic any (V2 Double)
worldModel = proc _ -> do
  xPos <- particlePosition1D -< ()
  yPos <- particlePosition1D -< ()
  returnA -< V2 xPos yPos

observationModel :: System Stochastic State Observation
observationModel = proc latent -> do
  xNoise <- whiteNoise -< ()
  yNoise <- whiteNoise -< ()
  returnA -< latent + V2 xNoise yNoise

posteriorDistribution :: System (Stochastic & Unnormalized) Observation State
posteriorDistribution = proc obs -> do
  latent <- worldModel -< ()
  observe -< (normalPdf2D obs 1 latent)
  returnA -< latent

inferredPosterior :: System Stochastic Observation [(State, Log Double)]
inferredPosterior = particleFilter params posteriorDistribution










-----------------------
-- second example
-----------------------

posteriorDistributionA :: System (Stochastic & Unnormalized) (Observation, AgentAction i) State
posteriorDistributionA = proc (obs, action) -> do
  latent <- worldModel -< action
  observe -< (normalPdf2D obs 1 latent)
  returnA -< latent


initialAction :: AgentAction i
initialAction = undefined

actionModel :: System Stochastic [(State, Log Double)] (AgentAction i)
actionModel = undefined

agent :: AgentID i -> System Stochastic Observation (AgentAction i) 
agent _ = feedback initialAction proc (obs, oldAction) -> do
  belief <- particleFilter params posteriorDistributionA -< (obs, oldAction)
  newAction <- actionModel -< belief
  returnA -< (newAction, newAction)

demo2 :: System (Stochastic & Feedback) UserInput Picture
demo2 = proc _ -> do
  rec
    observation <- worldModel -< action 
    action <- agent one -< observation
  returnA -< mempty

-----------------------
-- third example
-----------------------


demo3 :: System (Stochastic & Feedback) UserInput Picture
demo3 = proc _ -> do
  rec
    observation <- trueWorldModel -< (action1, action2) 
    action1 <- agent one -< observation
    action2 <- agent two -< observation
  returnA -< error "todo: visualization"


-----------------------
-- fourth example
-----------------------



demo4 :: System (Stochastic & Feedback) UserInput Picture
demo4 = proc _ -> do
  rec
    observation <- trueWorldModel -< (action1, action2) 
    action1 <- cleverAgent one -< observation
    action2 <- cleverAgent two -< observation
  returnA -< error "todo: visualize"

trueWorldModel :: System Stochastic (AgentAction One, AgentAction Two) State
trueWorldModel = undefined


cleverAgent :: AgentID i -> System Stochastic Observation (AgentAction i) 
cleverAgent agentID = proc obs -> do
  belief <- particleFilter params (cleverPosteriorDistribution agentID) -< obs
  action <- cleverActionModel agentID -< belief
  returnA -< action

  where 

    

    cleverPosteriorDistribution :: AgentID i -> System (Stochastic & Unnormalized) Observation State
    cleverPosteriorDistribution agentID = proc obs -> do
      latent <- cleverPriorDistribution agentID -< undefined
      observe -< (normalPdf2D obs 1 latent)
      returnA -< latent

    -- note: this isn't quite right, but for demonstrative purposes...
    cleverPriorDistribution :: forall i. AgentID i -> System Stochastic (AgentAction i) State
    cleverPriorDistribution agentID = feedback initialAction proc (myAction, otherAgentAction :: AgentAction (Other i)) -> do
        state <- trueWorldModel -< case agentID of 
            SOne -> (myAction, otherAgentAction)
            STwo -> (otherAgentAction, myAction)

        obs <- observationModel -< state
        newOtherAgentAction <- agent (other agentID) -< obs 

        returnA -< (state, newOtherAgentAction)

    cleverActionModel :: AgentID i -> System Stochastic [(State, Log Double)] (AgentAction i)
    cleverActionModel agentID = undefined

    