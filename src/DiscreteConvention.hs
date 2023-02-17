{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module DiscreteConvention where

import Linear (_y)
import Control.Monad.Bayes.Class (MonadDistribution (normal, random, uniformD, bernoulli), condition)
import Data.Vector ()
import Example (Result (..), empirical, prior, drawBall)
import FRP.Rhine.Gloss
    ( constM, green, yellow, violet, black )
import Inference
  ( SMCSettings (n), particleFilter, params
  )
import Linear.V2 (V2 (..))
import Numeric.Log (Log (ln))
import Prelude hiding (id, (.), until)
import Control.Lens
import Data.MonadicStreamFunction ( arrM )
import Control.Arrow
import Concurrent (UserInput)
import Graphics.Gloss (Picture, blue, text)
import Data.Singletons
import Data.Singletons.TH ( genSingletons )
import Data.Maybe (fromMaybe)
import GUI (ButtonConfig(..), button, buttonParams, slider)
import FRP.Rhine.Gloss (red)
import Control.Category ( Category((.), id) )
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import FRP.Rhine.Gloss (translate)
import FRP.Rhine.Gloss (scale)
import FRP.Rhine.Gloss (Color, rectangleSolid)
import FRP.Rhine.Gloss (withAlpha)
import Witch (into)
import FRP.Rhine.Gloss (color)
import Util hiding (runNSteps)
import Decision (stepper)
import Data.MonadicStreamFunction.InternalCore
import Debug.Trace (traceM)


-- Use dependent types to make agent general across Agent One and Agent Two
data AgentNumber = One | Two
genSingletons [''AgentNumber]

newtype AgentAction (i :: AgentNumber) = AgentAction {_agentAction :: Maybe Color} deriving Show
$(makeLenses ''AgentAction)


data Observation where
  Observation :: {_stateObs :: V2 Double, _action1Obs :: AgentAction One, _action2Obs :: AgentAction Two} -> Observation
$(makeLenses ''Observation)

data Mode = RedMeansAboveBlueMeansBelow | BlueMeansAboveRedMeansBelow | NothingMeansAnything deriving (Eq, Show)

newtype Language where
    Language :: {_mode :: Mode} -> Language
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

type World = SignalFunction Stochastic ((AgentAction One, AgentAction Two), (Double, Mode)) (State, Observation)
type WorldFromPerspectiveOf i = AgentID i -> SignalFunction Stochastic (AgentAction i, (Double, Mode)) (State, Observation)
type Agent (i :: AgentNumber) = AgentID i -> SignalFunction Stochastic (Observation, (Double, Mode)) (AgentAction i, Particles State)
type AgentID i = Sing (i :: AgentNumber)



type family Other (i :: AgentNumber) where
    Other One = Two
    Other Two = One

other :: AgentID i -> AgentID (Other i)
other SOne = STwo
other STwo = SOne

agentLens :: AgentID i -> Lens' (AgentAction One, AgentAction Two) (AgentAction i)
agentLens i = case i of
    SOne -> _1
    STwo -> _2

actionObs :: AgentID i -> Lens' Observation (AgentAction i)
actionObs i = case i of
            SOne -> action1Obs
            STwo -> action2Obs



-- depth of mutual recursion between Agent One and Agent Two
newtype Depth = Depth {_depth :: Int}  deriving (Eq, Show, Num) via Int

-- std :: Double
-- std = 2.0

noise :: SignalFunction Stochastic Double Double
noise = arrM (normal 0 )

numParticles :: Int
numParticles = 50

---- main code


movement :: SignalFunction Stochastic (AgentAction One, AgentAction Two) State
movement = proc acts -> do
    pos <- Example.prior -< ()
    -- lang <- constM (uniformD [RedMeansAboveBlueMeansBelow, BlueMeansAboveRedMeansBelow]) -< ()
    lang <- (\case True -> RedMeansAboveBlueMeansBelow; False ->    BlueMeansAboveRedMeansBelow) <$> bernoulliProcess False 0.001 -< ()
    -- delayedActions <- iPre (AgentAction Nothing, AgentAction Nothing) -< acts 
    returnA -< State
                    { _ball = pos,
                    _actions = acts,
                    _language = Language (lang)
                    }

observationModel :: SignalFunction Stochastic (State, Double) Observation
observationModel = proc (state, std) -> do
    (n1, n2) <- noise &&& noise -< std
    returnA -< Observation {
            _stateObs = state ^. ball + V2 n1 n2,
            _action1Obs = state ^. actions . _1,
            _action2Obs = state ^. actions . _2
            }


world :: World
world = proc (acts, (std, _)) -> do
    trueState <- movement -< acts
    observation <- observationModel -< (trueState, std)
    returnA -< (trueState, observation)


agentIPrior :: forall i . Depth -> WorldFromPerspectiveOf i
agentIPrior (Depth 0) agentID = proc (action, (std, md)) -> do

    ballPos@(V2 ballX ballY) <- Example.prior -< ()
    actionPrior1 <- constM (uniformD [red, blue]) -< ()
    actionPrior2 <- constM (uniformD [red, blue]) -< ()
    -- = Just $ V2 ballX ballY + uncurry V2 noise
    lang <- (\case True -> RedMeansAboveBlueMeansBelow; False -> BlueMeansAboveRedMeansBelow) <$> bernoulliProcess False 0.001 -< ()
    -- performOnFirstSample (uniformD $ (constM . pure) <$> [RedMeansAboveBlueMeansBelow, BlueMeansAboveRedMeansBelow]) -< ()
    -- let lang = RedMeansAboveBlueMeansBelow

    (n1, n2) <- noise &&& noise -< std

    actObsI <- arrM (\case
        (bp, RedMeansAboveBlueMeansBelow)
                | bp ^. _y > 0 -> (\case True -> Just red; False -> Just blue) <$> bernoulli 0.9
                | bp ^. _y < 0 -> (\case True -> Just red; False -> Just blue) <$> bernoulli 0.1
        (bp, BlueMeansAboveRedMeansBelow)
                | bp ^. _y > 0 -> (\case True -> Just red; False -> Just blue) <$> bernoulli 0.1
                | bp ^. _y < 0 -> (\case True -> Just red; False -> Just blue) <$> bernoulli 0.9
        _ -> undefined
        ) -< (ballPos, lang)

    let observation = Observation {
            _stateObs = ballPos + V2 n1 n2,
            _action1Obs = AgentAction actObsI
                 ,
            _action2Obs = AgentAction actObsI
            }    

    returnA -<
        (State {_ball = V2 ballX ballY, _actions =
            case agentID of
                SOne -> (action, AgentAction (Just actionPrior2))
                STwo -> (AgentAction (Just actionPrior1), action),
            _language = Language $ case agentID of 
                SOne -> lang
                STwo -> md
                }

        , observation)
agentIPrior (Depth d) agentID = feedback (AgentAction (Just 0) :: AgentAction (Other i))
    proc (((action, (std, md)), otherAgentAction)) -> do

        state <- movement -< case agentID of
            SOne -> (action, otherAgentAction)
            STwo -> (otherAgentAction, action)
        obs <- observationModel -< (state, std)
        (newOtherAgentAction, _) <- agentI (Depth (d-1)) (other agentID) -< (obs, (std, md))
        returnA -< ((state, obs), newOtherAgentAction)



agentIPosterior :: Depth -> AgentID i -> SignalFunction (Stochastic & Unnormalized) (Observation, AgentAction i, (Double, Mode)) State
agentIPosterior 0  i = proc (observation, agentIAction, (std, md)) -> do
    (statePrediction, obsPrediction) <- agentIPrior (Depth 0) i -< (agentIAction, (std, md))
    observe -< normalPdf2D (statePrediction ^. ball) std (observation ^. stateObs)
    case observation ^. actionObs i . agentAction of 
        Just o -> arrM condition -< obsPrediction ^. actionObs i . agentAction == Just o
        Nothing -> returnA -< ()
    
    
    -- let utt = observation ^. actionObs (other i) . agentAction
    -- let meaning = case (statePrediction ^. language . mode, utt) of 
    --         (RedMeansAboveBlueMeansBelow, Just c) 
    --                 | c==red -> statePrediction ^. ball . _y > 0
    --                 | c==blue -> statePrediction ^. ball . _y < 0
    --         (BlueMeansAboveRedMeansBelow, Just c) 
    --                 | c==red -> statePrediction ^. ball . _y < 0
    --                 | c==blue -> statePrediction ^. ball . _y > 0
    --         _ -> True
    -- observe -< if meaning then 1 else 0.1
    -- -- arrM condition -< meaning 
    
    returnA -< statePrediction

agentIPosterior depth i = proc (observation, agentIAction, (std, md)) -> do
    (statePrediction, obsPrediction) <- agentIPrior depth i -< (agentIAction, (std, md))
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

-- runNSteps _ 0 input = pure input 
runNSteps (MSF msf) 1 input = 
    fst <$> msf input
runNSteps (MSF msf) n input = do
    (mid, nextMsf) <- msf input
    runNSteps nextMsf (n-1) input -- (n-1) mid
    

decisionModelAgentI :: Depth -> AgentID i ->
    SignalFunction (Stochastic & Unnormalized) (State, (AgentAction i), (Double, Mode)) (AgentAction i)
decisionModelAgentI (Depth 0) i = proc (state, prevAct, ps@(std, md)) -> do

    -- x <- arr expected -< state ^. ball
    -- return -< undefined
    -- nextAct <- constM (uniformD $ AgentAction <$> [Just red, Just blue]) -< ()
    nextAct <- (\case True ->AgentAction (Just red); False -> AgentAction (Just blue)) <$> bernoulliProcess False 0.001 -< ()
    (_,  nextStep) <- stepper (agentIPrior (Depth 0) i) -< (prevAct, (ps))
    (futureState :: State, _) <- arrM id -< runNSteps nextStep 1 (nextAct, ps)
    arrM condition -< futureState ^. actions . agentLens ( i) . agentAction == Just red
    -- arrM traceM -< show (nextAct, futureState, case i of SOne -> 1; _ -> 2)
    -- arrM condition -< nextAct ^. agentAction == Just red
    returnA -< nextAct

    -- action <- AgentAction <$> Example.prior -< ()
    -- (outcome, _) <- agentIPrior (Depth 0) i -< action
    -- -- case i of 
    -- --     SOne -> 
    -- observe -< normalPdf2D (outcome ^. actions . agentLens i . agentAction) 0.1 3
    -- returnA -< action
decisionModelAgentI (Depth d) i = proc (state, prevAct, ps) -> do
    -- action <- AgentAction <$> Example.prior -< ()

    nextAct <- (\case True ->AgentAction (Just red); False -> AgentAction (Just blue)) <$> bernoulliProcess False 0.001 -< ()
    (_,  nextStep) <- stepper (agentIPrior (Depth d) i) -< (prevAct, (ps))
    (futureState :: State, _) <- arrM id -< runNSteps nextStep 2 (nextAct, ps)
    arrM condition -< futureState ^. actions . agentLens (other i) . agentAction == Just red
    -- arrM traceM -< show (nextAct, futureState, case i of SOne -> 1; _ -> 2)
    -- arrM condition -< nextAct ^. agentAction == Just red
    returnA -< nextAct



    -- belief <- particleFilter params{n=10} $ agentIPosterior (Depth 0) (other i) -< (makeObs (action ^. agentAction) i, AgentAction 0)
    -- belief' <- arr (unsafePerformIO . sampleIO . empirical) -< belief
    -- observe -< normalPdf2D (belief' ^. ball) 0.001 (-3)

    -- -- (outcome, _) <- agentIPrior (Depth (d)) i -< action
    -- -- observe -< normalPdf2D (outcome ^. actions . agentLens (other i) . agentAction) 0.001 (-3)
    -- -- observe -< normalPdf2D (outcome ^. ball) 0.1 (state ^. ball)
    -- returnA -< action
    -- undefined -< undefined

    -- where

    -- makeObs :: V2 Double -> AgentID i -> Observation
    -- makeObs action SOne = Observation {_action1Obs = AgentAction action}
    -- makeObs action STwo = Observation {_action2Obs = AgentAction action}


agentI :: Depth -> Agent i
agentI depth i = feedback (AgentAction $ Just 0) proc ((observation, ps), act) -> do



    belief <- particleFilter params {n=numParticles} (agentIPosterior depth i) -< (observation, act, ps)
    belief' <- arrM empirical -< belief
    -- nextAct <- particleFilter params{n=numParticles} $ decisionModelAgentI depth i -< (belief', act, ps)
    -- nextAct' <- arrM empirical -< nextAct
    
    
    
    nextAct' <- case depth of
        Depth i -> returnA -< 
            AgentAction case (belief' ^. language . mode, belief' ^. ball . _y .to (> 0) ) of 
                    (RedMeansAboveBlueMeansBelow, True) -> Just red
                    (RedMeansAboveBlueMeansBelow, False) -> Just blue
                    (BlueMeansAboveRedMeansBelow, True) -> Just blue
                    (BlueMeansAboveRedMeansBelow, False) -> Just red
                    _ -> Nothing
            -- first (AgentAction . Just . (^. ball)) <$> belief
        _ -> undefined -< undefined




            -- particleFilter params{n=numParticles} (decisionModelAgentI depth i) -< belief'
    -- nextAct <- arr (Just . AgentAction . expected) -< first (^. agentAction) <$> action
    -- let encoded = nextAct & agentAction . _Just %~ (+ belief' ^. language . coords)
    returnA -<  ((nextAct', belief), nextAct')

main :: SignalFunction Stochastic UserInput Picture
main = feedback (AgentAction Nothing, AgentAction Nothing) proc (userInput, actions) -> do

    (sliderPic, r) <- slider (V2 (-400) 300) 60 -< userInput
    let std = r + 0.01
    (buttonPic, buttonOn) <- button buttonParams{
        buttonPos=V2 (-400) (-300),
        buttonColor=red} -< userInput
    (buttonPic2, buttonOn2) <- button buttonParams{
        buttonPos=V2 (400) (-300),
        buttonColor=blue} -< userInput
    let md = if buttonOn2 then RedMeansAboveBlueMeansBelow else BlueMeansAboveRedMeansBelow
    (trueState, trueObservation) <- world -< (actions, (std, md))

    (a2actNew, beliefAgent2) <- agentI (Depth 0) STwo -< (trueObservation, (std, md))
    (a1actNew, beliefAgent1) <- agentI (Depth 0) SOne -< (trueObservation, (std, md))

    let newActions = if buttonOn then (a1actNew, a2actNew) else (AgentAction Nothing, AgentAction Nothing)

    pic1 <- renderObjects violet -< Result {
        measured = trueObservation ^. stateObs,
        latent = trueState ^. ball,
        particles = []
        }

    pic2 <- renderObjects green -< Result {
        measured = 1000,
        latent = 1000,
        -- latent = newActions ^. _1 . agentAction ,
        particles =  first (^. ball) <$>  beliefAgent1
        }

    pic3 <- renderObjects yellow -< Result {
        measured = 1000,
        latent = 1000, -- newActions ^. _2 . agentAction . to (fromMaybe 1000),
        particles =  first (^. ball) <$> beliefAgent2
        }

    beliefAgent1' <- arrM empirical -< beliefAgent1
    beliefAgent2' <- arrM empirical -< beliefAgent2
    let pic4 = translate (200) (-100) $ scale 0.2 0.2 $ text $ show $ beliefAgent1' ^. language . mode
    let pic5 = translate (-200) (-100) $ scale 0.2 0.2 $ text $ show $ beliefAgent2' ^. language . mode

    pic6 <- drawSquare -< (V2 2 2, 1, fromMaybe black (a1actNew ^. agentAction))
    pic7 <- drawSquare -< (V2 (-2) 2, 1, fromMaybe black (a2actNew ^. agentAction))

    -- pic4 <- fold <$> mapMSF drawParticle -< (\(x,y) -> (x,y,green)) . first (^. language . coords) <$> beliefAgent1
    -- pic5 <- fold <$> mapMSF drawParticle -< (\(x,y) -> (x,y,yellow)) . first (^. language . coords) <$> beliefAgent2

    -- let languagePic = translate 400 300 $ scale 0.5 0.5 (pic4 <> pic5)


    returnA -< (pic1
        <> pic2 <> pic3 
            -- <> languagePic 
        <> pic4 <> pic5
        <> pic6 <> pic7
            <> buttonPic <> buttonPic2 <> sliderPic, newActions)



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
  