{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module FixedConvention where


import Control.Monad.Bayes.Class (MonadDistribution (normal, random))
import Data.Vector ()
import Example ( Result (..), empirical, prior, drawBall, decayingIntegral)
import FRP.Rhine.Gloss
    ( constM,
      green,
      yellow,
      violet,
      translate,
      scale,
      Color,
      rectangleSolid,
      withAlpha,
      color )
import Inference
  ( SMCSettings (n), particleFilter, params
  )
import Linear.V2 (V2 (..), _x, _y)
import Numeric.Log (Log (ln))
import Prelude hiding ((.), until)
import Control.Lens
    ( Lens', Field1(_1), Field2(_2), (^.), makeLenses )
import Data.MonadicStreamFunction ( arrM, feedback, MSF, accumulateWith )
import Control.Arrow ( returnA, Arrow(first, (&&&), arr) )
import Concurrent (UserInput)
import Graphics.Gloss (Picture, line)
import Data.Singletons ( Sing )
import Data.Singletons.TH ( genSingletons )
import Control.Category ( Category((.)) )
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import Witch (into)
import Util
    ( Particles,
      type (>-/->),
      type (>-->),
      constantly,
      normalPdf2D,
      observe )
import Control.Monad.Trans.MSF (performOnFirstSample)
import Demo (oscillator)
import Linear.V

---
-- quick syntax primer
---


newtype A where
  B :: {unB :: Double} -> A
-- means that 
-- 1. A is a type
-- 2. B is a function of type (Double -> A)
-- 3. unB is a function of type (A -> Double)
-- Note: A and Double are isomorphic types, but distinguished by the type checker. (B 4) + 4 won't typecheck.


-- Use dependent types to make agent general across Agent One and Agent Two
data AgentNumber = One | Two
genSingletons [''AgentNumber]

newtype AgentAction (i :: AgentNumber) where
  AgentAction :: {_agentAction :: V2 Double} -> AgentAction i
  deriving Show
$(makeLenses ''AgentAction)

data State where
  State :: {_ball :: V2 Double,
              _actions :: (AgentAction One, AgentAction Two)}
             -> State
  deriving Show
-- a simple macro, not important
$(makeLenses ''State)

data Observation where
  Observation :: {_stateObs :: V2 Double, _action1Obs :: AgentAction One, _action2Obs :: AgentAction Two} -> Observation
$(makeLenses ''Observation)

newtype Convention where
    Convention :: {_coords :: V2 Double} -> Convention
    deriving Show
$(makeLenses ''Convention)


type AgentID i = Sing (i :: AgentNumber)
type UserData = Bool

------------------
-- dependent types 
------------------

type family Other (i :: AgentNumber) where
    Other One = Two
    Other Two = One

other :: AgentID i -> AgentID (Other i)
other SOne = STwo
other STwo = SOne


one :: SAgentNumber 'One
one = SOne
two :: SAgentNumber 'Two
two = STwo

pattern AgentOne :: SAgentNumber 'One
pattern AgentOne = SOne
pattern AgentTwo :: SAgentNumber 'Two
pattern AgentTwo = STwo

agentLens :: AgentID i -> Lens' (AgentAction One, AgentAction Two) (AgentAction i)
agentLens i = case i of
    SOne -> _1
    STwo -> _2

actionObs :: AgentID i -> Lens' Observation (AgentAction i)
actionObs i = case i of
            SOne -> action1Obs
            STwo -> action2Obs

------------------
-- params 
------------------

std :: Double
std = 0.1

noise :: Double >--> Double
noise = arrM (normal 0)

numParticles :: Int
numParticles = 50


------------------
-- utils 
------------------


walk1D :: Double >--> Double
walk1D = proc std -> do
    dacceleration <- arrM (normal 0 ) -< std
    acceleration <- decayingIntegral 1 -< dacceleration
    velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
    position <- decayingIntegral 1 -< velocity
    returnA -< position

walk1DL :: Double >--> Double
walk1DL = proc std -> do
    dacceleration <- arrM (normal 0 ) -< std
    acceleration <- decayingIntegral 0.75 -< dacceleration
    velocity <- decayingIntegral 0.75 -< acceleration -- Integral, dying off exponentially
    position <- decayingIntegral 0.75 -< velocity
    returnA -< position

-- depth of mutual recursion between Agent One and Agent Two
newtype Depth = Depth {_depth :: Int}  deriving (Eq, Show, Num) via Int



------------------
---- the world
------------------


conventionPrior :: AgentID i -> () >--> Convention
conventionPrior _ = proc () -> do
    oscillation <- oscillator 1 -< ()
    convention :: V2 Double <- performOnFirstSample (do
                                            x <- normal 0 1
                                            y <- normal 0 1
                                            return (constM (pure $ V2 x y))) -< ()
    returnA -< Convention (convention + oscillation)

stateModel :: (AgentAction One, AgentAction Two) >--> State
stateModel = proc actions -> do
    ballPos <- Example.prior -< ()
    returnA -< State
                    { _ball = ballPos,
                    _actions = actions
                    }

observationModel :: State >--> Observation
observationModel = proc state -> do
    (n1, n2) <- noise &&& noise -< 0.1
    (n1A1, n2A1) <- noise &&& noise -< 0.5
    (n1A2, n2A2) <- noise &&& noise -< 0.5
    returnA -< Observation {
            _stateObs = state ^. ball + V2 n1 n2,
            _action1Obs = AgentAction (state ^. actions . _1 . agentAction + V2 n1A1 n2A1),
            _action2Obs = AgentAction (state ^. actions . _2 . agentAction + V2 n1A2 n2A2)
            }

world :: (AgentAction One, AgentAction Two) >--> (State, Observation)
world = proc actions -> do
    trueState <- stateModel -< actions
    observation <- observationModel -< trueState
    returnA -< (trueState, observation)



------------------
---- the agent
------------------

agentIPrior :: forall i . Depth -> AgentID i -> (AgentAction i, Convention) >--> State
agentIPrior d agentID = feedback (AgentAction 0 :: AgentAction (Other i))
    proc ((action, convention), otherAgentAction) -> do
        state <- stateModel -< case agentID of
            SOne -> (action, otherAgentAction)
            STwo -> (otherAgentAction, action)
        obs <- observationModel -< state
        (newOtherAgentAction, _) <-
            constantly empirical .
            particleFilter params{n=2} (agent (d-1) (other agentID))
            -< (obs, convention)
        returnA -< (state, newOtherAgentAction)

agentIPosterior :: Depth -> AgentID i -> ((Observation, Convention), AgentAction i) >-/-> State
agentIPosterior depth agentID = proc ((observation, convention), agentIAction) -> do
    statePred <- agentIPrior depth agentID -< (agentIAction, convention)
    observe -< normalPdf2D (observation ^. stateObs) std (statePred ^. ball)
    let action = observation ^. actionObs (other agentID) . agentAction
    let predictedAction = statePred ^. actions.agentLens (other agentID) . agentAction
    observe -< normalPdf2D action 0.5 predictedAction
    returnA -< statePred

agentIDecision :: Depth -> AgentID i -> (State, Convention) >-/-> AgentAction i
agentIDecision _ _ = proc (state, convention) -> do 
    returnA -< AgentAction $ state ^. ball + convention ^. coords

agent :: Depth -> AgentID i -> (Observation, Convention) >-/-> (AgentAction i, State)
agent 0 _ = proc (obs, convention) -> do
    s <- arr ((/10) . sum . take 10) . accumulateWith (:) [] -< obs ^. stateObs
    let state = State s (obs ^. actionObs SOne, obs ^. actionObs STwo )
    returnA -< (AgentAction (s + convention ^. coords), state)
agent depth i = feedback (AgentAction 0) proc input@((_, convention), _) -> do
    state <- agentIPosterior depth i -< input
    nextAct <- agentIDecision depth i -< (state, convention) 
    -- let nextAct = AgentAction $ state ^. ball + convention ^. coords
    returnA -<  ((nextAct, state), nextAct)

joint :: Depth -> AgentID i -> Observation >-/-> ((AgentAction i, State), Convention)
joint depth agentID = proc obs -> do
    convention <- conventionPrior agentID -< ()
    (action, state) <- agent depth agentID -< (obs, convention)
    returnA -< ((action, state), convention)





------------------
---- rendering
------------------


main :: UserInput >--> Picture
main = feedback (AgentAction 0, AgentAction 0) proc (_, actions) -> do

    (trueState, trueObservation) <- world -< actions

    agent2Belief <- particleFilter params{n=50} (joint (Depth 1) STwo) -< trueObservation
    ((a2actNew, _), _) <- constantly empirical -< agent2Belief
    let stateBeliefAgent2 = first (snd . fst) <$> agent2Belief
    let conventionBeliefAgent2 = first snd <$> agent2Belief

    agent1Belief <- particleFilter params{n=50} (joint (Depth 1) SOne) -< trueObservation
    ((a1actNew, _), _) <- constantly empirical -< agent1Belief
    let stateBeliefAgent1 = first (snd . fst) <$> agent1Belief
    let conventionBeliefAgent1 = first snd <$> agent1Belief

    let newActions = (a1actNew, a2actNew)

    pic1 <- renderObjects violet -< Result {
        measured = V2
            (trueObservation ^. stateObs . _x)
            (trueObservation ^. stateObs . _y),
        latent = trueState ^. ball,
        particles = []
        }

    pic2 <- renderObjects green -< Result {
        measured = 1000, -- a1actNew ^. agentAction . to (fromMaybe 1000),
        latent = 1000, -- newActions ^. _1 . agentAction . to (fromMaybe 1000),
        particles =  first (^. ball) <$> stateBeliefAgent1
        }

    pic3 <- renderObjects yellow -< Result {
        measured = 1000, --  a2actNew ^. agentAction . to (fromMaybe 1000),
        latent = 1000, -- newActions ^. _2 . agentAction . to (fromMaybe 1000),
        particles = first (^. ball) <$> stateBeliefAgent2
        }

    pic4 <- fold <$> mapMSF drawParticle -< (\(x,y) -> (x,y,green)) . first (^. coords) <$> conventionBeliefAgent1
    pic5 <- fold <$> mapMSF drawParticle -< (\(x,y) -> (x,y,yellow)) . first (^. coords) <$> conventionBeliefAgent2
    let languagePic = translate 300 0 $ scale 0.5 0.5 (pic4 <> pic5)





    returnA -< (
            translate (-300) 0 (pic1 <> pic2 <> pic3)
            <> line [(0, 1000), (0,-1000)]
            <> languagePic
            , newActions)





















--------- utils

drawSquare :: Monad m => MSF m (V2 Double, Double, Color) Picture
drawSquare = proc (V2 x y, width, theColor) -> do
    returnA -<
        scale 150 150 $
        translate (into @Float x) (into @Float y) $
        color theColor $
        rectangleSolid
            (into @Float width)
            (into @Float width)

drawParticle ::  Monad m => MSF m (V2 Double, Log Double, Color) Picture
drawParticle = proc (position, probability, col) -> do
  drawSquare -< (position, 0.05, withAlpha (into @Float $ exp $ 0.2 * ln probability) col)



renderObjects ::  Monad m => Color -> MSF m Result Picture
renderObjects col = proc Result { particles, measured, latent} -> do

    observation <- drawBall -< (measured, 0.05, col)
    ball <- drawBall -< (latent, 0.1, col)
    parts <- fold <$> mapMSF drawParticle' -< (\(x,y) -> (x,y,col)) <$> particles
    returnA -< (observation <> ball <> parts)


drawParticle' ::  Monad m => MSF m (V2 Double, Log Double, Color) Picture
drawParticle' = proc (position, probability, col) -> do
    drawBall -< (position, 0.05, withAlpha (into @Float $ exp $ 0.2 * ln probability) col)

uniformRectangle :: () >--> V2 Double
uniformRectangle = proc () -> do
    x <- constM random -< ()
    y <- constM random -< ()
    returnA -< V2 ((x - 0.5) * 5) ((y - 0.5) * 5)


-- core feature is that the level-zero agent mistakenly believes the convention to be part of the model of nature
-- interesting strong effect: the output from the interlocutor must be consistent with the output from the modelled interlocutor: but is this currently enforced??
