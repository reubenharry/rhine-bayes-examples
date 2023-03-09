
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module DoublyDiscreteConvention where

import Linear (_y)
import Control.Monad.Bayes.Class (MonadDistribution (normal, random, uniformD, bernoulli, categorical), condition)
import Data.Vector ()
import Example (Result (..), empirical, prior, drawBall)
import FRP.Rhine.Gloss
    ( constM,
      green,
      yellow,
      violet,
      black,
      red,
      translate,
      scale,
      Color,
      rectangleSolid,
      withAlpha,
      color )
import Inference
  ( SMCSettings (n), particleFilter, params
  )
import Linear.V2 (V2 (..))
import Numeric.Log (Log (ln))
import Prelude hiding (id, (.), until)
import Control.Lens
import Data.MonadicStreamFunction ( arrM, accumulateWith, iPre )
import Control.Arrow hiding (right, left)
import Concurrent (UserInput)
import Graphics.Gloss (Picture, blue, text, mixColors, pictures)
import Data.Singletons
import Data.Singletons.TH ( genSingletons )
import Data.Maybe (fromMaybe)
import GUI (ButtonConfig(..), button, buttonParams, slider)
import Control.Category ( Category((.), id) )
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import Witch (into)
import Util
import Decision (stepper)
import Data.MonadicStreamFunction.InternalCore
import Debug.Trace (traceM)
import qualified Data.Map as M
import Data.List (sortOn)
import Data.Ord (Down(..))
import GHC.IO (unsafePerformIO)
import Control.Monad.Trans.MSF (performOnFirstSample)
import qualified Data.Vector as V
import Control.Monad.Bayes.Population (Population, runPopulation)


-- render state as a square


-- direct a0
-- visible actions
-- continuous state with separate action signal space

-- Use dependent types to make agent general across Agent One and Agent Two
data AgentNumber = One | Two
genSingletons [''AgentNumber]

data StateColor = RedColor | BlueColor deriving (Eq, Ord, Show)

data SubAction = ToRed | ToBlue | DoNothing deriving (Eq, Ord, Show)

data AgentAction (i :: AgentNumber) = AgentAction {_leftAction :: SubAction,
    _rightAction :: SubAction} deriving (Show, Eq, Ord)
$(makeLenses ''AgentAction)

data Mode = C1 | C2 deriving (Eq, Show)


data State = State
  { _left :: StateColor,
    _right :: StateColor
  }
  deriving (Show, Eq, Ord)


data Observation where
  Observation :: {_leftObs :: StateColor, _rightObs :: StateColor} -> Observation
  deriving (Eq, Show)
$(makeLenses ''Observation)


-- a population of particles
type Particles a = [(a, Log Double)]


type UserData = Bool

-- a simple macro, not important
$(makeLenses ''State)

type (>-->) a b = SignalFunction Stochastic a b
type (>-/->) a b = SignalFunction (Stochastic & Unnormalized) a b

type AgentID i = Sing (i :: AgentNumber)
type World = ((AgentAction One, AgentAction Two), UserData) >--> (State, Observation)
type WorldFromPerspectiveOf i = AgentID i ->
    ((AgentAction i, Mode, UserData) >--> (State, Observation))
type Agent (i :: AgentNumber) = AgentID i ->
    ((Observation, Mode, UserData) >--> (AgentAction i, Particles State, M.Map (AgentAction i, (State, State)) (Log Double)))
type JointAgent (i :: AgentNumber) = AgentID i ->
    ((Observation, UserData) >--> (AgentAction i, Particles (State, Mode)))

type Actions = (AgentAction One, AgentAction Two)

type family Other (i :: AgentNumber) where
    Other One = Two
    Other Two = One

other :: AgentID i -> AgentID (Other i)
other SOne = STwo
other STwo = SOne


-- depth of mutual recursion between Agent One and Agent Two
newtype Depth = Depth {_depth :: Int}  deriving (Eq, Show, Num) via Int


numParticles :: Int
numParticles = 50

-- runSubAction :: SubAction -> StateColor -> StateColor
-- runSubAction DoNothing = id
-- runSubAction ToRed = otherColor


otherColor :: StateColor -> StateColor
otherColor = \case
    RedColor -> BlueColor
    BlueColor -> RedColor

-- otherAction :: SubAction -> SubAction
-- otherAction = \case
--     DoNothing -> ToRed
--     ToRed -> DoNothing






---- main code

unloopedEvolve :: ((SubAction, SubAction), State) >--> State
unloopedEvolve = proc (action@(aL, aR), oldState@(State oldL oldR)) -> do  
    flipL <- transition  -< (aL, oldL)
    flipR <- transition-< (aR, oldR)
    arrM traceM -< show (flipL, flipR, oldState)
    returnA -< State 
            (if flipL then otherColor oldL else oldL) 
            (if flipR then otherColor oldR else oldR)
    
    where 
        transition = arrM (\case 
            (DoNothing, _) -> bernoulli 0.001
            (ToBlue, BlueColor) -> bernoulli 0.001
            (ToBlue, RedColor) -> bernoulli 0.3
            (ToRed, BlueColor) -> bernoulli 0.3
            (ToRed, RedColor) -> bernoulli 0.001
            )

unloopedMovement :: (Actions, State) >--> (State, State)
unloopedMovement = proc (acts@(actions1, actions2), oldState@(State oldL oldR)) -> do
    action@(aL, aR) <- selectActions -< acts
    
    newState <- unloopedEvolve -< (action, oldState )
    returnA -< (newState, newState)
    
    where

        selectActions = arrM (\(AgentAction a1L a1R, AgentAction a2L a2R) ->
                do  aL <- uniformD [a1L, a2L]
                    aR <- uniformD [a1R, a2R]
                    return (aL, aR))

movement :: (AgentAction One, AgentAction Two) >--> State
movement = feedback (State BlueColor BlueColor) unloopedMovement


observationModel :: State >--> Observation
observationModel = proc state -> do
    arrM (\(State l r) -> do
            flipL <- bernoulli 0.0001
            flipR <- bernoulli 0.0001
            pure $ Observation 
                (if flipL then otherColor l else l) 
                (if flipR then otherColor r else r)) 
                -< state

observationModel' :: State >--> Observation
observationModel' = proc state -> do
    arrM (\(State l r) -> do
            flipL <- bernoulli 0.0001
            flipR <- bernoulli 0.0001
            pure $
                Observation
                    (if flipR then otherColor r else r)
                    (if flipR then otherColor l else l)
                    ) -< state



world :: World
world = proc (acts, _) -> do
    trueState <- movement -< acts
    observation <- observationModel -< trueState
    returnA -< (trueState, observation)

agentIPriorUnlooped :: forall i . Depth -> AgentID i -> (((AgentAction i, Mode, UserData), State) >--> ((State, Observation), State))
agentIPriorUnlooped 0 _ = proc ((action@(AgentAction aL aR), md, b), oldState@(State oldL oldR)) -> do

    -- flipL <- constM $ bernoulli 0.01 -< ()
    -- flipR <- constM $ bernoulli 0.01 -< ()

    newState <- unloopedEvolve -< ((aL, aR), oldState)
    obs <- case md of
            C1 -> observationModel -< newState
            C2 -> observationModel' -< newState

    returnA -< ((newState, obs), newState)

agentIPriorUnlooped (Depth d) agentID = feedback (AgentAction DoNothing DoNothing :: AgentAction (Other i)) 
    proc (((action@(AgentAction aL aR), md, b), oldState), otherAgentAction) -> do
        
        (newState, _) <- unloopedMovement -< case agentID of
                    SOne -> ((action, otherAgentAction), oldState)
                    STwo -> ((otherAgentAction, action), oldState)
        obs <- case md of
            C1 -> observationModel -< newState
            C2 -> observationModel' -< newState
        (newOtherAgentAction, _, _) <- agentI (Depth (d-1)) (other agentID) -< (obs, md,b)
        returnA -< (((newState, obs), newState), newOtherAgentAction)



agentIPrior :: forall i . Depth -> WorldFromPerspectiveOf i
agentIPrior depth i = feedback (State BlueColor BlueColor) (agentIPriorUnlooped depth i)

agentIPosterior :: Depth -> AgentID i ->
    ((Observation, Mode, AgentAction i, UserData) >-/-> State)
agentIPosterior depth  i = proc (observation@(Observation c1 c2), mode, agentIAction, userData) -> do
    (statePrediction, obsPrediction) <- agentIPrior depth i -< (agentIAction, mode, userData)
    arrM condition -< observation == obsPrediction
    returnA -< statePrediction

agentIPosteriorJoint :: AgentID i ->
    ((Observation, AgentAction i, UserData) >-/-> (State, Mode))
agentIPosteriorJoint  i = proc (observation, agentIAction, userData) -> do
    mode <- (\case True -> C1; False ->    C2) <$> bernoulliProcess True 0.001 -< ()
    (statePrediction, obsPrediction) <- agentIPrior 1 i -< (agentIAction, mode, userData)
    arrM condition -< observation == obsPrediction
    returnA -< (statePrediction, mode)

agentIDecisionModel :: Depth -> AgentID i ->
    ((State, Mode, AgentAction i, UserData) >-/-> (AgentAction i, (State, State)))
agentIDecisionModel depth i = proc (state, mode, prevAct@(AgentAction aaL aaR), userData) -> do

    nextActL <- pickAction -<()
    nextActR <- pickAction -<()
    let nextAct = AgentAction nextActL nextActR
    ((futureState@(State fsL fsR), _), _) <- agentIPriorUnlooped depth i -< ((nextAct, mode, userData), state)
    
    arrM condition -< futureState ^. right == RedColor
    -- arrM condition -< futureState ^. left == BlueColor
    returnA -< (nextAct, (futureState, state))

    where
        pickAction = constM $ do
            i <- categorical $ V.fromList [0.99, 0.005, 0.005]
            pure $ fromMaybe undefined $ [DoNothing, ToRed, ToBlue] ^? ix i

agentI :: Depth -> Agent i
agentI depth i = feedback (AgentAction DoNothing DoNothing :: AgentAction i) proc ((observation, mode, ps), act) -> do

    beliefDist <- particleFilter params {n=numParticles} (agentIPosterior depth i) -< (observation, mode, act, ps)

    belief <- arrM empirical -< beliefDist
    nextActDist <- particleFilter params{n=numParticles} $
            agentIDecisionModel depth i -< (belief, mode, act, ps)
    (nextAct, _) <- arrM empirical -< nextActDist
    returnA -<  ((nextAct, beliefDist, groupsD nextActDist), nextAct)

agentIJoint :: JointAgent i
agentIJoint i = feedback (AgentAction DoNothing DoNothing :: AgentAction i) proc ((observation, ps), act) -> do


    belief <- particleFilter params {n=numParticles} (agentIPosteriorJoint i) -< (observation, act, ps)
    belief'@(inferredState, inferredMode) <- arrM empirical -< belief
    nextAct <- particleFilter params{n=numParticles} $
            agentIDecisionModel 1 i -< (inferredState, inferredMode, act, ps)
    (nextAct', _) <- arrM empirical -< nextAct

    returnA -<  ((nextAct', belief), nextAct')



main :: SignalFunction Stochastic UserInput Picture
main = feedback (AgentAction DoNothing DoNothing, AgentAction DoNothing DoNothing) proc (userInput, actions@(_, AgentAction a2L a2R)) -> do

    (buttonPic, buttonOn) <- button buttonParams{
      buttonPos=V2 (-400) (-300), buttonColor=red, buttonInitialVal=False} -< userInput

    (buttonPic2, buttonOn2) <- button buttonParams{
        buttonPos=V2 (-500) (-300), buttonColor=red} -< userInput


    -- stL <- (\case False -> RedColor; True -> BlueColor) <$> bernoulliProcess False 0.001 -< ()
    -- let stR = (if True then RedColor else BlueColor)
    -- trueState@(State trueStateL trueStateR) <- arr (\((aL, aR), (stL, stR)) -> State (runSubAction aL stL) (runSubAction aR stR)) -< ((a2L,a2R), (stL, stR))
    trueState@(State trueStateL trueStateR) <- movement -< actions

    -- trueState@(State trueStateL trueStateR) <- movement -< (actions)
    trueObservation@(Observation obsL obsR) <- observationModel -< trueState
    -- let trueObservation@(Observation obsL obsR) = Observation BlueColor BlueColor
    -- (trueState@(State trueStateL trueStateR), trueObservation@(Observation obsL obsR)) <- world -< (actions, ())

    let trueMode1 = if buttonOn then C2 else C1

    -- (a2actNew@(AgentAction a2CL a2CR) , beliefAgent2, _) <- agentI 0 STwo -< (trueObservation, C1, False)
    res@(a1actNew@(AgentAction a1CL a1CR), beliefAgent1, dist) <- agentI 1 SOne -< (trueObservation, trueMode1,buttonOn2)

    let (a2actNew@(AgentAction a2CL a2CR) , beliefAgent2, _) = res

    -- inferredState1 <- arrM empirical -< beliefAgent1
    -- inferredState2 <- arrM empirical -< beliefAgent2

    let newActions = 
            -- (AgentAction DoNothing DoNothing, AgentAction DoNothing DoNothing) 
            (a1actNew, AgentAction (a2actNew ^. leftAction) (a2actNew ^. rightAction))

    let showPairOfColors (c1,c2) =
            let square1 = color (case c1 of RedColor -> red; BlueColor -> blue)
                    $ translate (-100) 0
                    $ rectangleSolid 50 50
                square2 = color (case c2 of RedColor -> red; BlueColor -> blue)
                    $ translate 100 0
                    $ rectangleSolid 50 50
            in square1 <> square2
        showPairOfActions (c1,c2) =
            let t1 =
                    translate (-100) 0
                    $ scale 0.2 0.2
                    $ text (case c1 of 
                            DoNothing -> "Do Nothing"
                            ToRed -> "ToRed"
                            ToBlue -> "ToBlue")
                t2 =
                    translate 100 0
                    $ scale 0.2 0.2
                    $ text (case c2 of 
                            DoNothing -> "Do Nothing"
                            ToRed -> "ToRed"
                            ToBlue -> "ToBlue")

            in t1 <> t2


        -- m = (M.fromList [("1", ()), ("2", ())]) & ix "1" .~ ()

        showBelief :: [(StateColor, Log Double)] -> Picture
        showBelief d =
            let amounts = foldr (\(col,amount) dict ->
                    dict & ix col %~ (+(exp $ ln amount))
                    ) (M.fromList [(BlueColor, 0 :: Double), (RedColor, 0)]) d
                redAmount = fromMaybe undefined $ amounts ^? ix RedColor
                blueAmount = fromMaybe undefined $ amounts ^? ix BlueColor
            in color (mixColors (into @Float redAmount) (into @Float blueAmount) red blue) $ rectangleSolid 10 10
        -- (\entry dict -> dict & at "blue" ~. Just 1) M.empty

        -- ifoldMap (\i (col, weight) -> color (withAlpha (into @Float $ ln $ exp weight) (toCol col)) (translate (fromIntegral i*10) 0 (rectangleSolid 10 10)) )

    let
        pic1 = translate (-200) (-100) $ showPairOfActions (a2CL, a2CR)
        pic2 = translate 200 (-100) $ showPairOfActions (a1CL, a1CR)
        pic3 = translate 0 100 $ showPairOfColors (trueStateL, trueStateR)
        pic3' = translate (-100) 150 $ showBelief (first (^. left) <$> beliefAgent1)
        pic3'' = translate 100 150 $ showBelief (first (^. right) <$> beliefAgent1)
        pic3''' = translate (-100) 170 $ showBelief (first (^. left) <$> beliefAgent2)
        pic3'''' = translate 100 170 $ showBelief (first (^. right) <$> beliefAgent2)
        pic4 = translate 0 200 $ showPairOfColors (obsL, obsR)
        pic5 = translate (-200) (-200) $ scale 0.2 0.2 $ text $ show trueMode1
        pic6 = translate (-600) (-200) $ scale 0.2 0.2 $ text $ show C1



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
        <> translate (-500) 0 ( (pictures [translate 0 (20*i) $ scale 0.1 0.1 $ text (show line) | (i, line) <- zip [0..] $ M.toList dist]))
        ,
        --  <> pic3 
        --     -- <> languagePic 
        -- <> pic4 <> pic5
        -- <> pic6 <> pic7
            -- <> buttonPic <> buttonPic2 <> sliderPic, 
        newActions)




mostFrequent :: Ord a => [a] -> a
mostFrequent = fst . head . sortOn (Down . snd) . M.toList . groups

groups :: Ord a => [a] -> M.Map a Int
groups = foldr 
    (\item dict -> dict & at item %~ (\case Nothing -> Just 0; Just x -> Just (x+1))) 
    M.empty

groupsD :: Ord a => [(a, Log Double)] -> M.Map a (Log Double)
groupsD = foldr 
    (\(item, w) dict -> dict & at item %~ (\case Nothing -> Just 0; Just x -> Just (x+w))) 
    M.empty

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


-- argmax :: Population m a -> a
-- argmax pop = let population = runPopulation pop
--     in undefined

    -- proc (AgentAction aL' aR', mode, ()) -> do

--     aL <- arrM (\a -> do
--         flipL <- bernoulli 0.1
--         pure if flipL then otherAction a else a) -< aL'
--     aR <- arrM (\a -> do
--         flipR <- bernoulli 0.1
--         pure if flipR then otherAction a else a) -< aR'

--     stL <- (\case False -> RedColor; True -> BlueColor) <$> bernoulliProcess False 0.001 -< ()
--     stR <- (\case False -> RedColor; True -> BlueColor) <$> bernoulliProcess False 0.001 -< ()
--     s <- arr (\((aL, aR), (stL, stR)) -> State (runSubAction aL stL) (runSubAction aR stR)) -< ((aL,aR), (stL, stR))
--     o <- case mode of
--         C1 -> observationModel -< s
--         C2 -> observationModel' -< s
--         _ -> error "nothing means anything" -< error "nothing means anything"
--     returnA -< (s,o)










    -- nextAct'' <- arr mostFrequent . arr (take 20) . accumulateWith (:) [] -< nextAct'
    -- let nextAct'' = if False then AgentAction ToRed DoNothing else AgentAction DoNothing DoNothing



    -- nextAct' <- case depth of
    --     Depth i -> returnA -< 
    --         AgentAction case (belief' ^. language . mode, belief' ^. ball . _y .to (> 0) ) of 
    --                 (C1, True) -> Just red
    --                 (C1, False) -> Just blue
    --                 (C2, True) -> Just blue
    --                 (C2, False) -> Just red
    --                 _ -> Nothing
    --         -- first (AgentAction . Just . (^. ball)) <$> belief
    --     _ -> undefined -< undefined




            -- particleFilter params{n=numParticles} (agentIDecisionModel depth i) -< belief'
    -- nextAct <- arr (Just . AgentAction . expected) -< first (^. agentAction) <$> action
    -- let encoded = nextAct & agentAction . _Just %~ (+ belief' ^. language . coords)