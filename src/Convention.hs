{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Convention where


import Control.Monad.Bayes.Class (MonadDistribution (normal, random))
import Data.Vector ()
import Example (walk1D, Result (..), empirical, prior, drawBall)
import FRP.Rhine.Gloss
    ( constM, green, yellow, violet )
import Inference
  ( SMCSettings (n), particleFilter, params
  )
import Linear.V2 (V2 (..))
import Numeric.Log (Log (ln))
import Prelude hiding ((.), until)
import Control.Lens
import Data.MonadicStreamFunction ( arrM, feedback )
import Control.Arrow
import Concurrent (UserInput)
import Graphics.Gloss (Picture)
import Data.Singletons
import Data.Singletons.TH ( genSingletons )
import Data.Maybe (fromMaybe)
import GUI (ButtonConfig(..), button, buttonParams)
import FRP.Rhine.Gloss (red)
import Control.Category
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import FRP.Rhine.Gloss (translate)
import FRP.Rhine.Gloss (scale)
import Data.MonadicStreamFunction (MSF)
import FRP.Rhine.Gloss (Color, rectangleSolid)
import FRP.Rhine.Gloss (withAlpha)
import Witch (into)
import FRP.Rhine.Gloss (color)
import Util


-- Use dependent types to make agent general across Agent One and Agent Two
data AgentNumber = One | Two
genSingletons [''AgentNumber]

newtype AgentAction (i :: AgentNumber) = AgentAction {_agentAction :: Maybe (V2 Double)} deriving Show
$(makeLenses ''AgentAction)


data Observation where
  Observation :: {_stateObs :: V2 Double, _action1Obs :: AgentAction One, _action2Obs :: AgentAction Two} -> Observation
$(makeLenses ''Observation)

newtype Language where
    Language :: {_coords :: V2 Double} -> Language
    deriving Show
$(makeLenses ''Language)

-- a population of particles
type Particles a = [(a, Log Double)]

data State = State
  { _ball :: V2 Double,
    _actions :: (AgentAction One, AgentAction Two),
    _language :: Language
  }
  deriving (Show)

-- a simple macro, not important
$(makeLenses ''State)

type World = SignalFunction Stochastic (AgentAction One, AgentAction Two) (State, Observation)
type WorldFromPerspectiveOf i = AgentID i -> SignalFunction Stochastic (AgentAction i) State
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
std = 2.0

noise :: SignalFunction Stochastic a Double
noise = constM (normal 0 std)

numParticles :: Int
numParticles = 50

---- main code


movement :: SignalFunction Stochastic (AgentAction One, AgentAction Two) State
movement = proc actions -> do
    -- ballX <- walk1D -< ()
    -- ballY <- walk1D -< ()
    k <- Example.prior -< ()
    lang <- walk1D &&& walk1D -< ()
    returnA -< State
                    { _ball = k,
                    _actions = actions,
                    _language = Language (uncurry V2 lang)
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
    -- ballX <- walk1D -< ()
    -- ballY <- walk1D -< ()
    V2 ballX ballY <- Example.prior -< ()
    lang <- walk1D &&& walk1D -< ()
    noise <- constM (normal 0 1) *** constM (normal 0 1) -< ((),())
    let actionPrior = Just $ V2 ballX ballY + uncurry V2 noise
    returnA -<
        (State {_ball = V2 ballX ballY, _actions =
            case agentID of
                SOne -> (action, AgentAction actionPrior)
                STwo -> (AgentAction actionPrior, action),
            _language = Language $ case agentID of 
                SOne -> uncurry V2 lang
                STwo -> uncurry V2 lang + 2
                }

        )
agentIPrior (Depth d) agentID = feedback (AgentAction (Just 0) :: AgentAction (Other i))
    proc (action, otherAgentAction) -> do

        state <- movement -< case agentID of
            SOne -> (action, otherAgentAction)
            STwo -> (otherAgentAction, action)
        obs <- observationModel -< state
        (newOtherAgentAction, ps) <- agentI (Depth (d-1)) (other agentID) -< obs
        returnA -< (state, newOtherAgentAction)


agentLens :: AgentID i -> Lens' (AgentAction One, AgentAction Two) (AgentAction i)
agentLens i = case i of
    SOne -> _1
    STwo -> _2

actionObs :: AgentID i -> Lens' Observation (AgentAction i)
actionObs i = case i of
            SOne -> action1Obs
            STwo -> action2Obs


agentIPosterior :: Depth -> AgentID i -> SignalFunction (Stochastic & Unnormalized) (Observation, AgentAction i) State
agentIPosterior 0  i = proc (observation, agentIAction) -> do
    statePrediction <- agentIPrior (Depth 0) i -< agentIAction
    observe -< normalPdf2D (statePrediction ^. ball) std (observation ^. stateObs)
    let utt = observation ^. actionObs (other i) . agentAction
    let meaning = fmap ((statePrediction ^. language . coords) `subtract`) utt
    -- meaning :: Maybe (V2 Double) <- arr (\(u,l) -> fmap (l `subtract`) u) -< (observation ^. actionObs (other i) . agentAction, lang)
    case meaning of
        Just k ->  observe -< normalPdf2D (statePrediction ^. ball) (std/2) k
        Nothing -> returnA -< ()
    returnA -< statePrediction

agentIPosterior depth i = proc (observation, agentIAction) -> do
    statePrediction <- agentIPrior depth i -< agentIAction
    observe -< normalPdf2D (statePrediction ^. ball) std (observation ^. stateObs)
    -- observe -< normalPdf2D
    --     (statePrediction ^. actions . agentLens (other i) . agentAction)
    --     (std/2)
    --     (observation ^. actionObs (other i) . agentAction)
    returnA -< statePrediction

uniformRectangle :: SignalFunction Stochastic () (V2 Double)
uniformRectangle = proc () -> do
    x <- constM random -< ()
    y <- constM random -< ()
    returnA -< V2 ((x - 0.5) * 5) ((y - 0.5) * 5)


decisionModelAgentI :: Depth -> AgentID i ->
    SignalFunction (Stochastic & Unnormalized) State (AgentAction i)
decisionModelAgentI (Depth 0) i = proc state -> do

    -- x <- arr expected -< state ^. ball
    -- return -< undefined
    undefined -< undefined

    -- action <- AgentAction <$> Example.prior -< ()
    -- (outcome, _) <- agentIPrior (Depth 0) i -< action
    -- -- case i of 
    -- --     SOne -> 
    -- observe -< normalPdf2D (outcome ^. actions . agentLens i . agentAction) 0.1 3
    -- returnA -< action
decisionModelAgentI (Depth d) i = proc state -> do
    -- action <- AgentAction <$> Example.prior -< ()



    -- belief <- particleFilter params{n=10} $ agentIPosterior (Depth 0) (other i) -< (makeObs (action ^. agentAction) i, AgentAction 0)
    -- belief' <- arr (unsafePerformIO . sampleIO . empirical) -< belief
    -- observe -< normalPdf2D (belief' ^. ball) 0.001 (-3)

    -- -- (outcome, _) <- agentIPrior (Depth (d)) i -< action
    -- -- observe -< normalPdf2D (outcome ^. actions . agentLens (other i) . agentAction) 0.001 (-3)
    -- -- observe -< normalPdf2D (outcome ^. ball) 0.1 (state ^. ball)
    -- returnA -< action
    undefined -< undefined

    where

    -- makeObs :: V2 Double -> AgentID i -> Observation
    -- makeObs action SOne = Observation {_action1Obs = AgentAction action}
    -- makeObs action STwo = Observation {_action2Obs = AgentAction action}


agentI :: Depth -> Agent i
agentI depth i = feedback (AgentAction $ Just 0) proc (observation, act) -> do



    belief <- particleFilter params {n=numParticles} (agentIPosterior depth i) -< (observation, act)
    belief' <- arrM empirical -< belief
    actionDist <- case depth of
        Depth 0 -> returnA -< first (AgentAction . Just . (^. ball)) <$> belief
        _ -> particleFilter params{n=numParticles} (decisionModelAgentI depth i) -< belief'
    -- nextAct <- arr (Just . AgentAction . expected) -< first (^. agentAction) <$> action
    nextAct <- arrM empirical -< actionDist
    let encoded = nextAct & agentAction . _Just %~ (+ belief' ^. language . coords)
    returnA -<  ((encoded, belief), encoded)

main :: SignalFunction Stochastic UserInput Picture
main = feedback (AgentAction Nothing, AgentAction Nothing) proc (userInput, actions) -> do

    (buttonPic, buttonOn) <- button buttonParams{
        buttonPos=V2 (-300) 400,
        buttonColor=red} -< userInput
    (trueState, trueObservation) <- world -< actions

    (a2actNew, beliefAgent2) <- agentI (Depth 0) STwo -< trueObservation
    (a1actNew, beliefAgent1) <- agentI (Depth 0) SOne -< trueObservation

    let newActions = if buttonOn then (a1actNew, a2actNew) else (AgentAction Nothing, AgentAction Nothing)

    pic1 <- renderObjects violet -< Result {
        measured = trueObservation ^. stateObs,
        latent = trueState ^. ball,
        particles = []
        }

    pic2 <- renderObjects green -< Result {
        measured = 1000,
        latent = newActions ^. _1 . agentAction . to (fromMaybe 1000),
        particles =  first (^. ball) <$>  beliefAgent1
        }

    pic3 <- renderObjects yellow -< Result {
        measured = 1000,
        latent = newActions ^. _2 . agentAction . to (fromMaybe 1000),
        particles =  first (^. ball) <$> beliefAgent2
        }

    pic4 <- fold <$> mapMSF drawParticle -< (\(x,y) -> (x,y,green)) . first (^. language . coords) <$> beliefAgent1
    pic5 <- fold <$> mapMSF drawParticle -< (\(x,y) -> (x,y,yellow)) . first (^. language . coords) <$> beliefAgent2

    let languagePic = translate 400 300 $ scale 0.5 0.5 (pic4 <> pic5)


    returnA -< (pic1
        <> pic2 <> pic3 <> languagePic <> buttonPic, newActions)



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
  