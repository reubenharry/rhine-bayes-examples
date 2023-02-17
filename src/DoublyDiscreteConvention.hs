
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module DoublyDiscreteConvention where

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
import Control.Arrow hiding (right, left)
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
import Util 
import Decision (stepper)
import Data.MonadicStreamFunction.InternalCore
import Debug.Trace (traceM)


-- Use dependent types to make agent general across Agent One and Agent Two
data AgentNumber = One | Two
genSingletons [''AgentNumber]

data StateColor = RedColor | BlueColor deriving (Eq, Show)

data SubAction = Toggle | DoNothing deriving Show

data AgentAction (i :: AgentNumber) = AgentAction {_leftAction :: SubAction, 
    _rightAction :: SubAction} deriving Show
$(makeLenses ''AgentAction)

data Mode = RedMeansRedBlueMeansBlue | BlueMeansRedRedMeansBlue | NothingMeansAnything deriving (Eq, Show)


data State = State
  { _left :: StateColor,
    _right :: StateColor
  }
  deriving (Show)


data Observation where
  Observation :: {_leftObs :: StateColor, _rightObs :: StateColor} -> Observation
  deriving (Eq, Show)
$(makeLenses ''Observation)


-- a population of particles
type Particles a = [(a, Log Double)]


type UserData = ()

-- a simple macro, not important
$(makeLenses ''State)

type (>-->) a b = SignalFunction Stochastic a b
type (>-/->) a b = SignalFunction (Stochastic & Unnormalized) a b

type AgentID i = Sing (i :: AgentNumber)
type World = ((AgentAction One, AgentAction Two), UserData) >--> (State, Observation)
type WorldFromPerspectiveOf i = AgentID i -> 
    ((AgentAction i, Mode, UserData) >--> (State, Observation))
type Agent (i :: AgentNumber) = AgentID i -> 
    ((Observation, Mode, UserData) >--> (AgentAction i, Particles State))
type JointAgent (i :: AgentNumber) = AgentID i -> 
    ((Observation, UserData) >--> (AgentAction i, Particles (State, Mode)))



type family Other (i :: AgentNumber) where
    Other One = Two
    Other Two = One

other :: AgentID i -> AgentID (Other i)
other SOne = STwo
other STwo = SOne


-- depth of mutual recursion between Agent One and Agent Two
newtype Depth = Depth {_depth :: Int}  deriving (Eq, Show, Num) via Int


numParticles :: Int
numParticles = 10

---- main code

runSubAction :: SubAction -> StateColor -> StateColor
runSubAction DoNothing = id
runSubAction Toggle = \case RedColor -> BlueColor; BlueColor -> RedColor

movement :: (AgentAction One, AgentAction Two) >--> State
movement = proc acts -> do
    (aL, aR) <-  arrM (\(AgentAction a1L a1R, AgentAction a2L a2R) -> 
        do 
            aL <- uniformD [a1L, a2L]
            aR <- uniformD [a1R, a2R]
            return (aL, aR)) -< acts

    stL <- (\case False -> RedColor; True -> BlueColor) <$> bernoulliProcess False 0.001 -< ()
    stR <- (\case False -> RedColor; True -> BlueColor) <$> bernoulliProcess False 0.001 -< ()
    state <- arr (\((aL, aR), (stL, stR)) -> State (runSubAction aL stL) (runSubAction aR stR)) -< ((aL,aR), (stL, stR))

    returnA -< state

otherColor :: StateColor -> StateColor
otherColor = \case
    RedColor -> BlueColor
    BlueColor -> RedColor

otherAction :: SubAction -> SubAction
otherAction = \case
    DoNothing -> Toggle
    Toggle -> DoNothing

observationModel :: State >--> Observation
observationModel = proc state -> do
    Observation lO rO <- arrM (\(State l r) -> do 
        do
            flipL <- bernoulli 0.001
            flipR <- bernoulli 0.001
            pure $ Observation (if flipL then otherColor l else l) (if flipR then otherColor r else r)) -< state
    returnA -< Observation {
            _leftObs = lO,
            _rightObs = rO
            }

observationModel' :: State >--> Observation
observationModel' = proc state -> do
    Observation lO rO <- arrM (\(State l r) -> do 
        do
            flipL <- bernoulli 0.001
            flipR <- bernoulli 0.001
            pure $ Observation (if not flipL then otherColor l else l) (if not flipR then otherColor r else r)) -< (state)
    returnA -< Observation {
            _leftObs = lO,
            _rightObs = rO
            }


world :: World
world = proc (acts, ()) -> do
    trueState <- movement -< acts
    observation <- observationModel -< trueState
    returnA -< (trueState, observation)


agentIPrior :: forall i . Depth -> WorldFromPerspectiveOf i
agentIPrior 0 _ = proc (AgentAction aL' aR', mode, ()) -> do

    aL <- arrM (\a -> do
        flip <- bernoulli 0.1
        pure if flip then otherAction a else a) -< aL' 
    aR <- arrM (\a -> do
        flip <- bernoulli 0.1
        pure if flip then otherAction a else a) -< aR' 

    stL <- (\case False -> RedColor; True -> BlueColor) <$> bernoulliProcess False 0.001 -< ()
    stR <- (\case False -> RedColor; True -> BlueColor) <$> bernoulliProcess False 0.001 -< ()
    s <- arr (\((aL, aR), (stL, stR)) -> State (runSubAction aL stL) (runSubAction aR stR)) -< ((aL,aR), (stL, stR))
    o <- case mode of 
        RedMeansRedBlueMeansBlue -> observationModel -< s
        BlueMeansRedRedMeansBlue -> observationModel' -< s
        _ -> error "nothing means anything" -< error "nothing means anything"
    returnA -< (s,o)
        

agentIPrior (Depth d) agentID = feedback (AgentAction DoNothing DoNothing :: AgentAction (Other i))
    proc ((action, mode, ()), otherAgentAction) -> do

        state <- movement -< case agentID of
            SOne -> (action, otherAgentAction)
            STwo -> (otherAgentAction, action)
        obs <- observationModel -< (state)
        (newOtherAgentAction, _) <- agentI (Depth (d-1)) (other agentID) -< (obs, mode, ())
        returnA -< ((state, obs), newOtherAgentAction)



agentIPosterior :: Depth -> AgentID i -> 
    ((Observation, Mode, AgentAction i, UserData) >-/-> State)
agentIPosterior depth  i = proc (observation, mode, agentIAction, userData@()) -> do
    (statePrediction, obsPrediction) <- agentIPrior depth i -< (agentIAction, mode, userData)
    arrM condition -< observation == obsPrediction
    returnA -< statePrediction

agentIPosteriorJoint :: AgentID i -> 
    ((Observation, AgentAction i, UserData) >-/-> (State, Mode))
agentIPosteriorJoint  i = proc (observation, agentIAction, userData@()) -> do
    mode <- (\case True -> RedMeansRedBlueMeansBlue; False ->    BlueMeansRedRedMeansBlue) <$> bernoulliProcess True 0.001 -< ()
    (statePrediction, obsPrediction) <- agentIPrior 1 i -< (agentIAction, mode, userData)
    arrM condition -< observation == obsPrediction
    returnA -< (statePrediction, mode)



decisionModelAgentI :: Depth -> AgentID i ->
    ((State, Mode, AgentAction i, UserData) >-/-> AgentAction i)
decisionModelAgentI depth i = proc (state, mode, prevAct, userData@()) -> do

    nextActL <- (\case True -> DoNothing; False -> Toggle) <$> bernoulliProcess True 0.01 -< ()
    nextActR <- (\case True -> DoNothing; False -> Toggle) <$> bernoulliProcess False 0.01 -< ()
    let nextAct = AgentAction nextActL nextActR
    (_,  nextStep) <- stepper (agentIPrior depth  i) -< (prevAct, mode, userData)
    (futureState :: State, _) <- arrM id -< runNSteps nextStep 2 (nextAct, mode, userData)
    arrM condition -< futureState ^. right == BlueColor
    -- arrM traceM -< show (nextAct, futureState, case i of SOne -> 1; _ -> 2)
    -- arrM condition -< nextAct ^. agentAction == Just red
    returnA -< nextAct


agentI :: Depth -> Agent i
agentI depth i = feedback (AgentAction DoNothing DoNothing :: AgentAction i) proc ((observation, mode, ps), act) -> do


    belief <- particleFilter params {n=numParticles} (agentIPosterior depth i) -< (observation, mode, act, ps)
    belief' <- arrM empirical -< belief
    nextAct <- particleFilter params{n=numParticles} $ 
            decisionModelAgentI depth i -< (belief', mode, act, ps)
    nextAct' <- arrM empirical -< nextAct
    
    
    
    -- nextAct' <- case depth of
    --     Depth i -> returnA -< 
    --         AgentAction case (belief' ^. language . mode, belief' ^. ball . _y .to (> 0) ) of 
    --                 (RedMeansRedBlueMeansBlue, True) -> Just red
    --                 (RedMeansRedBlueMeansBlue, False) -> Just blue
    --                 (BlueMeansRedRedMeansBlue, True) -> Just blue
    --                 (BlueMeansRedRedMeansBlue, False) -> Just red
    --                 _ -> Nothing
    --         -- first (AgentAction . Just . (^. ball)) <$> belief
    --     _ -> undefined -< undefined




            -- particleFilter params{n=numParticles} (decisionModelAgentI depth i) -< belief'
    -- nextAct <- arr (Just . AgentAction . expected) -< first (^. agentAction) <$> action
    -- let encoded = nextAct & agentAction . _Just %~ (+ belief' ^. language . coords)
    returnA -<  ((nextAct', belief), nextAct')

agentIJoint :: JointAgent i
agentIJoint i = feedback (AgentAction DoNothing DoNothing :: AgentAction i) proc ((observation, ps), act) -> do

   
    belief <- particleFilter params {n=numParticles} (agentIPosteriorJoint i) -< (observation, act, ps)
    belief'@(inferredState, inferredMode) <- arrM empirical -< belief
    nextAct <- particleFilter params{n=numParticles} $ 
            decisionModelAgentI 1 i -< (inferredState, inferredMode, act, ps)
    nextAct' <- arrM empirical -< nextAct
    
    returnA -<  ((nextAct', belief), nextAct')


main :: SignalFunction Stochastic UserInput Picture
main = feedback (AgentAction DoNothing DoNothing, AgentAction DoNothing DoNothing) proc (userInput, actions@(_, AgentAction a2L a2R)) -> do


    (buttonPic, buttonOn) <- button buttonParams{
        buttonPos=V2 (-400) (-300),
        buttonColor=red} -< userInput
    
    (buttonPic2, buttonOn2) <- button buttonParams{
        buttonPos=V2 (-500) (-300),
        buttonColor=red} -< userInput

        
    stL <- (\case False -> RedColor; True -> BlueColor) <$> bernoulliProcess False 0.001 -< ()
    let stR = (if buttonOn2 then RedColor else BlueColor) 
    trueState@(State trueStateL trueStateR) <- arr (\((aL, aR), (stL, stR)) -> State (runSubAction aL stL) (runSubAction aR stR)) -< ((a2L,a2R), (stL, stR))
    

    -- trueState@(State trueStateL trueStateR) <- movement -< (actions)
    trueObservation@(Observation obsL obsR) <- observationModel -< trueState
    -- (trueState@(State trueStateL trueStateR), trueObservation@(Observation obsL obsR)) <- world -< (actions, ())
    
    let trueMode1 = if buttonOn then RedMeansRedBlueMeansBlue else BlueMeansRedRedMeansBlue

    (a2actNew@(AgentAction a2CL a2CR) , beliefAgent2) <- agentI 0 STwo -< (trueObservation, RedMeansRedBlueMeansBlue, ())
    (a1actNew@(AgentAction a1CL a1CR), beliefAgent1) <- agentI 0 SOne -< (trueObservation, trueMode1, ())

    (inferredState1) <- arrM empirical -< beliefAgent1
    (inferredState2) <- arrM empirical -< beliefAgent2

    let newActions = (a1actNew, a2actNew) 

    let showPairOfColors (c1,c2) = 
            let square1 = color (case c1 of RedColor -> red; BlueColor -> blue) 
                    $ translate (-100) 0
                    $ rectangleSolid 50 50 
                square2 = color (case c2 of RedColor -> red; BlueColor -> blue) 
                    $ translate (100) 0
                    $ rectangleSolid 50 50 
            in square1 <> square2
        showPairOfActions (c1,c2) = 
            let t1 = 
                    translate (-100) 0
                    $ scale 0.2 0.2
                    $ text (case c1 of DoNothing -> "Do Nothing"; Toggle -> "Toggle")
                t2 = 
                    translate (100) 0
                    $ scale 0.2 0.2
                    $ text (case c2 of DoNothing -> "Do Nothing"; Toggle -> "Toggle")

            in t1 <> t2

        toCol = \case RedColor -> red; BlueColor -> blue

        showBelief :: [(StateColor, Log Double)] -> Picture
        showBelief b = ifoldMap (\i (col, weight) -> color (withAlpha (into @Float $ ln $ exp weight) (toCol (col))) (translate (fromIntegral i*10) 0 (rectangleSolid 10 10)) ) b

    let 
        pic1 = translate (-200) (-100) $ showPairOfActions (a2CL, a2CR)
        pic2 = translate (200) (-100) $ showPairOfActions (a1CL, a1CR)
        pic3 = translate (0) (100) $ showPairOfColors (trueStateL, trueStateR)
        pic3' = translate (-100) (150) $ showBelief (first (^. left) <$> beliefAgent1)
        pic3'' = translate (100) (150) $ showBelief (first (^. right) <$> beliefAgent1)
        pic3''' = translate (-100) (170) $ showBelief (first (^. left) <$> beliefAgent2)
        pic3'''' = translate (100) (170) $ showBelief (first (^. right) <$> beliefAgent2)
        pic4 = translate (0) (200) $ showPairOfColors (obsL, obsR)
        pic5 = translate (-200) (-200) $ scale 0.2 0.2 $ text $ show trueMode1
        pic6 = translate (-600) (-200) $ scale 0.2 0.2 $ text $ show RedMeansRedBlueMeansBlue
        
    

    returnA -< (
           pic1
        <> pic2
        <> pic3
        <> pic3'
        <> pic3''
        <> pic3'''
        <> pic3''''
        <> pic4
        <> pic5
        <> pic6
        <> buttonPic
        <> buttonPic2
        ,
        --  <> pic3 
        --     -- <> languagePic 
        -- <> pic4 <> pic5
        -- <> pic6 <> pic7
            -- <> buttonPic <> buttonPic2 <> sliderPic, 
        newActions)



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
  