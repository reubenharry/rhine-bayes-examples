{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module SimpleConvention where


import Control.Monad.Bayes.Class (MonadDistribution (normal, random, uniformD), normalPdf, MonadFactor (score), MonadMeasure)
import Data.Vector ()
import Example ( Result (..), empirical, prior, drawBall, decayingIntegral)
import FRP.Rhine.Gloss
    ( constM,
      green,
      yellow,
      violet,
      red,
      translate,
      scale,
      Color,
      rectangleSolid,
      withAlpha,
      color, VectorSpace (norm) )
import Inference
  ( SMCSettings (n), particleFilter, params
  )
import Linear.V2 (V2 (..), _x, _y)
import Numeric.Log (Log (ln))
import Prelude hiding ((.), until)
import Control.Lens
import Data.MonadicStreamFunction ( arrM, feedback, MSF, accumulateWith )
import Control.Arrow
import Concurrent (UserInput)
import Graphics.Gloss (Picture, line)
import Data.Singletons
import Data.Singletons.TH ( genSingletons )
import Data.Maybe (fromMaybe)
import GUI (ButtonConfig(..), button, buttonParams)
import Control.Category
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import Witch (into)
import Util
import Debug.Trace (traceM)
import Control.Monad.Trans.MSF (performOnFirstSample)


-- core feature is that the level-zero agent mistakenly believes the convention to be part of the model of nature
-- interesting strong effect: the output from the interlocutor must be consistent with the output from the modelled interlocutor: but is this currently enforced??

-- Use dependent types to make agent general across Agent One and Agent Two
data AgentNumber = One | Two
genSingletons [''AgentNumber]

newtype AgentAction (i :: AgentNumber) = AgentAction {_agentAction :: V2 Double} deriving Show
$(makeLenses ''AgentAction)


data Observation where
  Observation :: {_stateObs :: V2 Double, _action1Obs :: AgentAction One, _action2Obs :: AgentAction Two} -> Observation
$(makeLenses ''Observation)

newtype Convention where
    Convention :: {_coords :: V2 Double} -> Convention
    deriving Show
$(makeLenses ''Convention)

data State = State
  { _ball :: V2 Double,
    _actions :: (AgentAction One, AgentAction Two)
  }
  deriving Show
-- a simple macro, not important
$(makeLenses ''State)

type WorldFromPerspectiveOf i = AgentID i -> AgentAction i >--> (State, Convention)
type Agent (i :: AgentNumber) = AgentID i -> Observation >--> (AgentAction i, Particles (State, Convention))
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

noise :: a >--> Double
noise = constM (normal 0 std)

numParticles :: Int
numParticles = 50


------------------
-- utils 
------------------


brownianMotion1D :: Double >--> Double
brownianMotion1D = proc std -> do
    dacceleration <- arrM (normal 0 ) -< std
    acceleration <- decayingIntegral 1 -< dacceleration
    velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
    position <- decayingIntegral 1 -< velocity
    returnA -< position

brownianMotion1DL :: Double >--> Double
brownianMotion1DL = proc std -> do
    dacceleration <- arrM (normal 0 ) -< std
    acceleration <- decayingIntegral 0.75 -< dacceleration
    velocity <- decayingIntegral 0.75 -< acceleration -- Integral, dying off exponentially
    position <- decayingIntegral 0.75 -< velocity
    returnA -< position

-- depth of mutual recursion between Agent One and Agent Two
newtype Depth = Depth {_depth :: Int}  deriving (Eq, Show, Num) via Int



------------------
---- main code
------------------


stateModel :: (AgentAction One, AgentAction Two) >--> State
stateModel = proc actions -> do
    ballPos <- Example.prior -< ()
    returnA -< State
                    { _ball = ballPos,
                    _actions = actions
                    }

observationModel :: State >--> Observation
observationModel = proc state -> do
    n1 <- noise -< ()
    n2 <- noise -< ()
    returnA -< Observation {
            _stateObs = state ^. ball + V2 n1 n2,
            _action1Obs = state ^. actions . _1,
            _action2Obs = state ^. actions . _2
            }


world :: (AgentAction One, AgentAction Two) >--> (State, Observation)
world = proc actions -> do
    trueState <- stateModel -< actions
    observation <- observationModel -< trueState
    returnA -< (trueState, observation)

conventionPrior :: AgentID i -> () >--> Convention
conventionPrior agentID = proc () -> do
    -- returnA -< Convention (V2 0 0)

    lang' <- brownianMotion1DL &&& brownianMotion1DL -< 10
    -- lang'' :: V2 Double <- performOnFirstSample (uniformD [ constM (pure (i)) | i <- [-0.2,-0.1,0,0.1, 0.2]]) -< ()
    lang'' :: V2 Double <- performOnFirstSample (do
                                            x <- normal 0 1
                                            y <- normal 0 1
                                            return (constM (pure $ V2 x y))) -< ()
    let lang = case agentID of
                SOne ->
                        -- uncurry V2 lang'  + 2
                        lang'' + uncurry V2 lang'
                STwo ->
                    lang'' + uncurry V2 lang' 
    returnA -< Convention lang

agentIPrior :: forall i . Depth -> WorldFromPerspectiveOf i
agentIPrior (Depth d) agentID =
    feedback (AgentAction 0 :: AgentAction (Other i))
    proc (action, otherAgentAction) -> do
        state <- stateModel -< case agentID of
            SOne -> (action, otherAgentAction)
            STwo -> (otherAgentAction, action)
        convention <- conventionPrior agentID -< ()
        obs <- observationModel -< state
        (newOtherAgentAction, _) <-
            -- agentI (Depth (d-1)) (other agentID) -< obs
            simpleAgent (other agentID) -< (obs, convention)
        returnA -< ((state, convention), newOtherAgentAction)

simpleAgent :: AgentID i -> (Observation, Convention) >--> (AgentAction i, Particles State)
-- AgentID i -> Observation >--> AgentAction i
simpleAgent agentID = proc (obs, convention) -> do
    s <- arr ((/10) . sum . take 10) . accumulateWith (:) [] -< obs ^. stateObs
    let state = State s (obs ^. actionObs SOne, obs ^. actionObs STwo )
    returnA -< (AgentAction (s + convention ^. coords), [(state, 1.0)])

agentIPosterior :: Depth -> AgentID i -> (Observation, AgentAction i) >-/-> (State, Convention)
agentIPosterior depth agentID = proc (observation, agentIAction) -> do
    joint@(statePred, _) <- agentIPrior depth agentID -< agentIAction

    observe -< normalPdf2D (observation ^. stateObs) std (statePred ^. ball)

    let action = observation ^. actionObs (other agentID) . agentAction
    let predictedAction = statePred ^. actions.agentLens (other agentID) . agentAction
    observe -< normalPdf2D action 1 predictedAction
    -- assert old action is new action

    returnA -< joint





agentI :: Depth -> Agent i
agentI depth i = feedback (AgentAction 0) proc (observation, act) -> do


    beliefDist <- particleFilter params{n=50} (agentIPosterior depth i) -< (observation, act)
    (state, convention) <- constantly empirical -< beliefDist
    -- actionDist <- case depth of
    --     Depth 0 -> returnA -< first (AgentAction . (^. _1 . ball)) <$> beliefDist
    --     _ -> particleFilter params{n=numParticles} (undefined depth i) -< state
    -- nextAct <- constantly empirical -< actionDist
    let nextAct = AgentAction $ state ^. ball + convention ^. coords
    returnA -<  ((nextAct, beliefDist), nextAct)




main :: UserInput >--> Picture
main = feedback (AgentAction 0, AgentAction 0) proc (_, actions) -> do


    (trueState, trueObservation) <- world -< actions

    (a2actNew, beliefAgent2) <- agentI (Depth 0) STwo -< trueObservation
    (a1actNew, beliefAgent1) <- agentI (Depth 0) SOne -< trueObservation
    -- (a1actNew, beliefAgent1) <- simpleAgent SOne -< (trueObservation, Convention 0)

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
        particles =  first ((^. ball) . fst) <$>  beliefAgent1
        }

    pic3 <- renderObjects yellow -< Result {
        measured = 1000, --  a2actNew ^. agentAction . to (fromMaybe 1000),
        latent = 1000, -- newActions ^. _2 . agentAction . to (fromMaybe 1000),
        particles =  first ((^. ball) . fst) <$> beliefAgent2
        }

    pic4 <- fold <$> mapMSF drawParticle -< (\(x,y) -> (x,y,green)) . first ((^. coords) . snd) <$> beliefAgent1
    pic5 <- fold <$> mapMSF drawParticle -< (\(x,y) -> (x,y,yellow)) . first ((^. coords) . snd) <$> beliefAgent2

    let languagePic = translate 300 0 $ scale 0.5 0.5 (pic4 <> pic5)
        -- pic4 <> 


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
