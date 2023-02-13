{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Convention where
import Control.Monad.Bayes.Class (MonadDistribution (normal, random), normalPdf)
import Data.Vector ()
import Example (drawBall, walk1D, Result (..), empirical, prior, posterior)
import FRP.Rhine.Gloss
    ( Arrow((&&&)), constM, returnA, green, yellow, violet, Color, withAlpha )
import GHC.TypeLits (Nat)
import Inference
  ( SMCSettings (n), particleFilter, params
  )
import Linear.V2 (V2 (..))
import Numeric.Log (Log (ln))
import Prelude hiding (until)
import Control.Lens
import Witch
import Data.Foldable (fold)
import Data.MonadicStreamFunction ( MSF, iPre, arrM, feedback )
import Control.Monad.Trans.MSF.List (mapMSF)
import Control.Arrow
import Data.Coerce (coerce)
import Concurrent (UserInput)
import Graphics.Gloss (Picture)
import Debug.Trace (traceM)
import Data.Singletons
import Data.Singletons.TH ( genSingletons )
import GHC.IO (unsafePerformIO)
import Control.Monad.Bayes.Sampler.Strict (sampleIO)
import Util

-- Use dependent types to make agent general across Agent One and Agent Two
data AgentNumber = One | Two
genSingletons [''AgentNumber]

newtype AgentAction (i :: AgentNumber) = AgentAction {_agentAction :: V2 Double} deriving Show
$(makeLenses ''AgentAction)


data Observation where
  Observation :: {_stateObs :: V2 Double, _action1Obs :: AgentAction One, _action2Obs :: AgentAction Two} -> Observation
$(makeLenses ''Observation)

-- a population of particles
type Particles a = [(a, Log Double)]

data State = State
  { _ball :: V2 Double,
    _actions :: (AgentAction One, AgentAction Two)
  }
  deriving (Show)

-- a simple macro, not important
$(makeLenses ''State)

type World = SignalFunction Stochastic (AgentAction One, AgentAction Two) (State, Observation)
type WorldFromPerspectiveOf i = AgentID i -> SignalFunction Stochastic (AgentAction i) (State, Observation)
type Agent (i :: AgentNumber) = AgentID i -> SignalFunction Stochastic Observation (AgentAction i, Particles State)
type AgentID i = Sing (i :: AgentNumber)



type family Other (i :: AgentNumber) where
    Other One = Two
    Other Two = One

other :: AgentID i -> AgentID (Other i)
other SOne = STwo
other STwo = SOne

-- depth of mutual recursion between Agent One and Agent Two
newtype Depth = Depth {_depth :: Int}  deriving (Eq, Show, Num) via Int

std :: Double
std = 0.5

type (⍈) a b = SignalFunction Stochastic a b

-- type (⍈) a b = SignalFunction Stochastic b a


noise :: a ⍈ Double
-- noise :: Double <-#-- a
noise = constM (normal 0 std)

numParticles :: Int
numParticles = 10

---- main code


movement :: SignalFunction Stochastic (AgentAction One, AgentAction Two) State
movement = proc actions -> do
    ballX <- walk1D -< ()
    ballY <- walk1D -< ()
    returnA -< State
                    { _ball = V2 ballX ballY,
                    _actions = actions
                    }

observationModel :: SignalFunction Stochastic State Observation
observationModel = proc state -> do
    (n1, n2) <- noise &&& noise -< ()
    returnA -< Observation {
            _stateObs = state ^. ball + V2 n1 n2,
            _action1Obs = state ^. actions . _1,
            _action2Obs = state ^. actions . _2
            }


world :: World
world = proc actions -> do
    trueState <- movement -< actions
    observation <- observationModel -< trueState
    returnA -< (trueState, observation)


agentIPrior :: forall i . Depth -> WorldFromPerspectiveOf i
agentIPrior (Depth 0) agentID = proc action -> do
    ballX <- walk1D -< ()
    ballY <- walk1D -< ()
    returnA -<
        -- V2 ballX ballY 
        (State {_ball = V2 ballX ballY, _actions =
            case agentID of
                SOne -> (action, AgentAction 0)
                STwo -> (AgentAction 0, action)
                },

        undefined)
agentIPrior (Depth d) agentID = feedback (AgentAction 0 :: AgentAction (Other i))
    proc (action, otherAgentAction) -> do

        state <- movement -< case agentID of
            SOne -> (action, otherAgentAction)
            STwo -> (otherAgentAction, action)
        obs <- observationModel -< state
        (newOtherAgentAction, ps) <- agentI (Depth (d-1)) (other agentID) -< obs
        returnA -< ((state, obs), newOtherAgentAction)


agentLens :: AgentID i -> Lens' (AgentAction One, AgentAction Two) (AgentAction i)
agentLens i = case i of
    SOne -> _1
    STwo -> _2

actionObs :: AgentID i -> Lens' Observation (AgentAction i)
actionObs i = case i of
            SOne -> action1Obs
            STwo -> action2Obs

agentIPosterior :: Depth -> AgentID i -> SignalFunction (Stochastic & Unnormalized) (Observation, AgentAction i) State
agentIPosterior 0 = \i -> proc (observation, agentIAction) -> do
    (statePrediction, _) <- agentIPrior (Depth 0) i -< agentIAction
    -- observe -< normalPdf2D (statePrediction ^. ball) std (observation ^. stateObs)
    observe -< normalPdf2D (statePrediction ^. ball) (std/10) (observation ^. actionObs (other i) . agentAction)
    returnA -< statePrediction
agentIPosterior depth = \i -> proc (observation, agentIAction) -> do
    (statePrediction, _) <- agentIPrior depth i -< agentIAction
    observe -< normalPdf2D (statePrediction ^. ball) std (observation ^. stateObs)
    observe -< normalPdf2D
        (statePrediction ^. actions . agentLens (other i) . agentAction)
        (std/2)
        (observation ^. actionObs (other i) . agentAction)
    returnA -< statePrediction

uniformRectangle :: SignalFunction Stochastic () (V2 Double)
uniformRectangle = proc () -> do
    x <- constM random -< ()
    y <- constM random -< ()
    returnA -< V2 ((x - 0.5) * 5) ((y - 0.5) * 5)


decisionModelAgentI :: Depth -> AgentID i ->
    SignalFunction (Stochastic & Unnormalized) State (AgentAction i)
decisionModelAgentI (Depth 0) = \i -> proc state -> do

    -- x <- arr expected -< state ^. ball
    -- return -< undefined
    undefined -< undefined

    -- action <- AgentAction <$> Example.prior -< ()
    -- (outcome, _) <- agentIPrior (Depth 0) i -< action
    -- -- case i of 
    -- --     SOne -> 
    -- observe -< normalPdf2D (outcome ^. actions . agentLens i . agentAction) 0.1 3
    -- returnA -< action
decisionModelAgentI (Depth d) = \i -> proc state -> do
    action <- AgentAction <$> Example.prior -< ()



    belief <- particleFilter params{n=10} $ agentIPosterior (Depth 0) (other i) -< (makeObs (action ^. agentAction) i, AgentAction 0)
    belief' <- arr (unsafePerformIO . sampleIO . empirical) -< belief
    observe -< normalPdf2D (belief' ^. ball) 0.001 (-3)

    -- (outcome, _) <- agentIPrior (Depth (d)) i -< action
    -- observe -< normalPdf2D (outcome ^. actions . agentLens (other i) . agentAction) 0.001 (-3)
    -- observe -< normalPdf2D (outcome ^. ball) 0.1 (state ^. ball)
    returnA -< action

    where

    makeObs :: V2 Double -> AgentID i -> Observation
    makeObs action SOne = Observation {_action1Obs = AgentAction action}
    makeObs action STwo = Observation {_action2Obs = AgentAction action}


agentI :: Depth -> Agent i
agentI depth i = feedback (AgentAction 0) proc (observation, act) -> do


    belief <- particleFilter params {n=numParticles} (agentIPosterior depth i) -< (observation, act)
    belief' <- arr painfulExpectation -< belief
    action <- case depth of
        Depth 0 -> returnA -< first (AgentAction . (^. ball)) <$> belief
        _ -> particleFilter params{n=numParticles} (decisionModelAgentI depth i) -< belief'
    nextAct <- arr (AgentAction . expected) -< first (^. agentAction) <$> action
    returnA -<  ((nextAct, belief), nextAct)

main :: SignalFunction Stochastic UserInput Picture
main = feedback (AgentAction 0, AgentAction 0) proc (userInput, actions) -> do


    (trueState, trueObservation) <- world -< actions

    (a2actNew, beliefAgent2) <- agentI (Depth 1) STwo -< trueObservation
    (a1actNew, beliefAgent1) <- agentI (Depth 0) SOne -< trueObservation


    pic1 <- renderObjects violet -< Result {
        measured = trueObservation ^. stateObs,
        latent = trueState ^. ball,
        particles = []
        }

    pic2 <- renderObjects green -< Result {
        measured = 1000,
        latent = a1actNew ^.  agentAction,
        particles =  first (^. ball) <$>  beliefAgent1
        }

    pic3 <- renderObjects yellow -< Result {
        measured = 1000,
        latent = a2actNew ^. agentAction,
        particles =  first (^. ball) <$> beliefAgent2
        }

    returnA -< (pic1
        <> pic2 <> pic3, (a1actNew, AgentAction 0))













painfulExpectation state =
    let f1 = first (^. ball) <$> state
        f2 = first (^. actions . _1 . agentAction) <$> state
        f3 = first (^. actions . _2 . agentAction) <$> state
    in State {_ball = expected f1, _actions = (AgentAction $ expected f2, AgentAction $ expected f3)}


renderObjects ::  Monad m => Color -> MSF m Result Picture
renderObjects col = proc Result { particles, measured, latent} -> do

    observation <- drawBall -< (measured, 0.05, col)
    ball <- drawBall -< (latent, 0.1, col)
    parts <- fold <$> mapMSF drawParticle -< (\(x,y) -> (x,y,col)) <$> particles
    returnA -< (observation <> ball <> parts)


drawParticle ::  Monad m => MSF m (V2 Double, Log Double, Color) Picture
drawParticle = proc (position, probability, col) -> do
    drawBall -< (position, 0.05, withAlpha (into @Float $ exp $ 0.2 * ln probability) col)