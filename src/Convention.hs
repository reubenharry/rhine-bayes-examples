{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Convention where


import Control.Monad.Bayes.Class (MonadDistribution (normal, random), normalPdf, MonadFactor (score), MonadMeasure)
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
import Data.MonadicStreamFunction ( arrM, feedback, MSF )
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
import Util hiding (Particles)
import Debug.Trace (traceM)


-- core feature is that the level-zero agent mistakenly believes the convention to be part of the model of nature

-- Use dependent types to make agent general across Agent One and Agent Two
data AgentNumber = One | Two
genSingletons [''AgentNumber]

newtype AgentAction (i :: AgentNumber) = AgentAction {_agentAction :: Maybe (V2 Double)} deriving Show
$(makeLenses ''AgentAction)


data Observation where
  Observation :: {_stateObs :: V2 (Maybe Double), _action1Obs :: AgentAction One, _action2Obs :: AgentAction Two} -> Observation
$(makeLenses ''Observation)

newtype Language where
    Language :: {_coords :: V2 Double} -> Language
    deriving Show
$(makeLenses ''Language)

-- a PopulationT of particles
type Particles a = [(a, Log Double)]

data State = State
  { _ball :: V2 Double,
    _actions :: (AgentAction One, AgentAction Two)
  }
  deriving (Show)

-- a simple macro, not important
$(makeLenses ''State)

type World = SignalFunction Stochastic (AgentAction One, AgentAction Two) (State, Observation)
type WorldFromPerspectiveOf i = AgentID i -> SignalFunction Stochastic (AgentAction i, UserData) (State, Language)
type Agent (i :: AgentNumber) = AgentID i -> SignalFunction Stochastic (Observation, UserData) (AgentAction i, Particles (State, Language))
type AgentID i = Sing (i :: AgentNumber)

type UserData = Bool

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

-- depth of mutual recursion between Agent One and Agent Two
newtype Depth = Depth {_depth :: Int}  deriving (Eq, Show, Num) via Int

std :: Double
std = 0.1

noise :: SignalFunction Stochastic a Double
noise = constM (normal 0 std)

numParticles :: Int
numParticles = 50

agentLens :: AgentID i -> Lens' (AgentAction One, AgentAction Two) (AgentAction i)
agentLens i = case i of
    SOne -> _1
    STwo -> _2

actionObs :: AgentID i -> Lens' Observation (AgentAction i)
actionObs i = case i of
            SOne -> action1Obs
            STwo -> action2Obs



brownianMotion1D :: SignalFunction Stochastic Double Double
brownianMotion1D = proc std -> do
    dacceleration <- arrM (normal 0 ) -< std
    acceleration <- decayingIntegral 1 -< dacceleration
    velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
    position <- decayingIntegral 1 -< velocity
    returnA -< position

---- main code


stateModel :: (AgentAction One, AgentAction Two) >--> State
stateModel = proc actions -> do
    ballPos <- Example.prior -< ()
    returnA -< State
                    { _ball = ballPos,
                    _actions = actions
                    }

observationModel :: SignalFunction Stochastic State Observation
observationModel = proc state -> do
    (n1, n2) <- noise &&& noise -< ()
    returnA -< Observation {
            _stateObs = Just <$> state ^. ball + V2 n1 n2,
            _action1Obs = state ^. actions . _1,
            _action2Obs = state ^. actions . _2
            }


world :: World
world = proc actions -> do
    trueState <- stateModel -< actions
    observation <- observationModel -< trueState
    returnA -< (trueState, observation)


agentIPrior :: forall i . Depth -> WorldFromPerspectiveOf i
agentIPrior (Depth 0) agentID = proc (action, staticPrior) -> do

    ballPos@(V2 ballX ballY) <- Example.prior -< ()
    lang' <- if staticPrior
                then brownianMotion1D &&& brownianMotion1D -< 8
                else constM (pure (0, 0)) -< ()
    let lang = case (agentID, staticPrior) of
                (_, False) -> uncurry V2 lang'
                (SOne, _) -> uncurry V2 lang' + 2
                (STwo, _) -> uncurry V2 lang'
    -- State {_language = lang, _ball = ballPos@(V2 ballX ballY)} <- stateModel -< undefined
    -- noise <- constM (normal 0 0.1) &&& constM (normal 0 0.1) -< ()
    let otherAction = Just $ V2 ballX ballY +
            -- uncurry V2 noise + 
            lang
    let actions = case agentID of
            SOne -> (action, AgentAction otherAction)
            STwo -> (AgentAction otherAction, action)
    returnA -< (State
                    { _ball = ballPos,
                    _actions = actions
                    }, Language lang)



agentIPrior (Depth d) agentID =
    feedback (AgentAction (Just 0) :: AgentAction (Other i))
    proc ((action, userData), otherAgentAction) -> do
        state <- stateModel -< case agentID of
            SOne -> (action, otherAgentAction)
            STwo -> (otherAgentAction, action)
        lang <- undefined -< undefined -- brownianMotion1D &&& brownianMotion1D -< 8
        obs <- observationModel -< state
        (newOtherAgentAction, _) <- agentI (Depth (d-1)) (other agentID) -< (obs, userData)
        returnA -< ((state, Language $ uncurry V2 lang), newOtherAgentAction)


observeMay :: SignalFunction (Stochastic & Unnormalized) (Maybe Double, Double) ()
observeMay = arrM \(obs, st) -> case obs of
        Nothing -> return ()
        Just o -> score $ normalPdf st std o

agentIPosterior :: Depth -> AgentID i ->
    SignalFunction (Stochastic & Unnormalized) (Observation, AgentAction i, UserData) (State, Language)
agentIPosterior depth agentID = proc (observation, agentIAction, userData) -> do
    joint@(statePred, _) <- agentIPrior depth agentID -< (agentIAction, userData)

    case observation ^. stateObs of
        -- Just o -> observe -< normalPdf2D (statePred ^. ball) std o
        (V2 oX oY) -> do
            observeMay -< (oX, statePred ^. ball . _x)
            observeMay -< (oY, statePred ^. ball . _y)

    let utt = observation ^. actionObs (other agentID) . agentAction
    let predictedUtt = statePred^.actions.agentLens (other agentID).agentAction
    case (utt, predictedUtt) of
            (Just u, Just u') -> observe -< normalPdf2D u 0.1 u'
            _ -> returnA -< ()
    -- assert old action is new action

    -- let meaning = ((statePrediction ^. language . coords) `subtract`) <$> utt
    -- case meaning of
    --     Just k ->  observe -< normalPdf2D (statePrediction ^. ball) (std*2) k
    --     Nothing -> returnA -< ()

    returnA -< joint
-- agentIPosterior depth i = proc (observation, agentIAction) -> do
--     joint@(statePred, langPred) <- agentIPrior depth i -< agentIAction
--     observe -< normalPdf2D (statePred ^. ball) std (observation ^. stateObs)
--     -- observe -< normalPdf2D
--     --     (statePred ^. actions . agentLens (other i) . agentAction)
--     --     (std/2)
--     --     (observation ^. actionObs (other i) . agentAction)
--     returnA -< joint



decisionModelAgentI :: Depth -> AgentID i ->
    SignalFunction (Stochastic & Unnormalized) (State, UserData) (AgentAction i)
decisionModelAgentI (Depth 0) i = proc (state, userData) -> do

    -- x <- arr expected -< state ^. ball
    -- return -< undefined
    action <- AgentAction . Just <$> Example.prior -< ()
    (predState, lang) <- agentIPrior 0 i -< (action, userData)
    arrM traceM -< show (action, state ^. ball, norm $ predState ^. ball - state ^. ball)
    observe -< normalPdf2D (predState ^. actions . agentLens i . agentAction . to (fromMaybe 0)) 0.1 (state ^. ball)
    returnA -< action
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
agentI depth i = feedback (AgentAction $ Just 0) proc ((observation, userData), act) -> do



    beliefDist <- particleFilter params {n=numParticles} (agentIPosterior depth i) -< (observation, act, userData)
    (stateBelief, langBelief) <- arrM empirical -< beliefDist
    actionDist <- case depth of
        Depth 0 -> returnA -< first (AgentAction . Just . (^. ball) . fst) <$> beliefDist
        _ -> particleFilter params{n=numParticles} (decisionModelAgentI depth i) -< (stateBelief, userData)
    nextAct <- arrM empirical -< actionDist
    let encoded = nextAct & agentAction . _Just %~ (+ langBelief ^. coords)
    returnA -<  ((encoded, beliefDist), encoded)

main :: SignalFunction Stochastic UserInput Picture
main = feedback (AgentAction Nothing, AgentAction Nothing) proc (userInput, actions) -> do

    (a2obsXPic, a2obsX) <- button buttonParams{
        buttonPos=V2 100 300, buttonColor=red, buttonText = "a2 obsX"} -< userInput
    (a2obsYPic, a2obsY) <- button buttonParams{
        buttonPos=V2 100 250, buttonColor=red, buttonText = "a2 obsY"} -< userInput
    (a1obsXPic, a1obsX) <- button buttonParams{
        buttonPos=V2 50 300, buttonColor=red, buttonText = "a1 obsX"} -< userInput
    (a1obsYPic, a1obsY) <- button buttonParams{
        buttonPos=V2 50 250, buttonColor=red, buttonText = "a1 obsY"} -< userInput

    (buttonPic, buttonOn) <- button buttonParams{
        buttonPos=V2 200 300, buttonColor=red, buttonText = "communicate"} -< userInput
    (buttonPic3, buttonOn3) <- button buttonParams{
        buttonPos=V2 300 300, buttonColor=red, buttonText = "show conv"} -< userInput
    (buttonPic4, buttonOn4) <- button buttonParams{
        buttonPos=V2 400 300, buttonColor=red, buttonText = "fixed conv"} -< userInput
    (trueState, trueObservation) <- world -< actions

    (a2actNew, beliefAgent2) <- agentI (Depth 0) STwo -< (case (a2obsX, a2obsY) of
        (True, True) -> trueObservation
            & stateObs . _x .~ Nothing
            & stateObs . _y .~ Nothing
        (False, True) -> trueObservation
            & stateObs . _y .~ Nothing
        (True, False) -> trueObservation
            & stateObs . _x .~ Nothing
        _ -> trueObservation

        , buttonOn4)

    (a1actNew, beliefAgent1) <- agentI (Depth 0) SOne -< (case (a1obsX, a1obsY) of
        (True, True) -> trueObservation
            & stateObs . _x .~ Nothing
            & stateObs . _y .~ Nothing
        (False, True) -> trueObservation
            & stateObs . _y .~ Nothing
        (True, False) -> trueObservation
            & stateObs . _x .~ Nothing
        _ -> trueObservation

        , buttonOn4)


    -- agentI (Depth 0) SOne -< (trueObservation, buttonOn4)

    let newActions = if buttonOn then (a1actNew, a2actNew) else (AgentAction Nothing, AgentAction Nothing)

    pic1 <- renderObjects violet -< Result {
        measured = V2
            (fromMaybe 100 (trueObservation ^. stateObs . _x))
            (fromMaybe 100 (trueObservation ^. stateObs . _y)),
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


    returnA -< (
            translate (-300) 0 (pic1 <> pic2 <> pic3)
            <> line [(0, 1000), (0,-1000)]
            <> (if buttonOn3 then languagePic else mempty)
            <> buttonPic
            <> a1obsXPic
            <> a1obsYPic
            <> a2obsXPic
            <> a2obsYPic
            <> buttonPic3 <> buttonPic4, newActions)





















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

uniformRectangle :: SignalFunction Stochastic () (V2 Double)
uniformRectangle = proc () -> do
    x <- constM random -< ()
    y <- constM random -< ()
    returnA -< V2 ((x - 0.5) * 5) ((y - 0.5) * 5)
