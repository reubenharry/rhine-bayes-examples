{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}

module Control where
import FRP.Rhine.Gloss hiding (Up, Down)
import Prelude hiding (until)
import qualified Example
import qualified Control.Monad.Morph as MM
import Linear.V2 (V2(..), _x, _y, angle, unangle)
import Control.Monad.Bayes.Class
import Numeric.Log
import Control.Monad.Bayes.Sampler
import Control.Monad.Morph
import Inference hiding (V2)
import Control.Monad.Bayes.Population (resampleMultinomial)
import Active (averageOf)
import Witch (into)
import Control.Lens
import Control.Monad (forever, void)
import Data.Fixed (mod')
import Control.Concurrent (newMVar, forkIO, swapMVar, MVar, readMVar)
import Text.Megaparsec (runParser, ParsecT, MonadParsec (eof))
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (float)
import qualified Linear as V
import qualified Data.Vector as VVV
import qualified Data.Vector as V
import Control.Monad.Trans.MSF (switch)


-- bouncing

std :: Double
std = 10


data State where
  State :: {_ball1Pos :: V2 Double,
            _ball2Pos :: V2 Double,
            _userSetting :: T.Text}
           -> State
  deriving Show

-- a simple macro, not important
$(makeLenses ''State)

newtype Angle = Angle Double deriving Show

mkAngle :: Double -> Angle
mkAngle a = Angle $ a `mod'` (2 * pi)

data AgentObservation = AgentObservation {_ball1Obs :: V2 Double, _userObs :: T.Text} deriving Show
$(makeLenses ''AgentObservation)

type UserInput = T.Text
type AgentInput = V2 Double
type Particles = [(State, Log Double)]
type UserObservation = State
data Direction = Up | Down


-- "Process Deterministic x y" is the type of signal functions 
-- where the input signal is (Time -> x)
-- and the output signal is (Time -> y)
-- So: (Time -> x) -> (Time -> y)
-- THIS IS NOT THE SAME AS A TIME-VARYING FUNCTION!! Time -> (x -> y)
-- "Process Stochastic x y" then is (effectively): (Time -> x) -> Distribution (Time -> y)





prior :: Process Stochastic AgentInput State
prior = movement where



    movement :: Process Stochastic AgentInput State
    movement  = feedback (State 0 0 "") proc (pos, oldState) -> do
        ball1PosAx <- walk1D -< ()
        ball1PosAy <- walk1D -< ()
        ball2Position@(V2 ball2PositionX ball2PositionY) <- iPre 0 >>> decayIntegral 1 -< pos
        ballPosX <- decayIntegral 1 >>> decayIntegral 1 -< ball1PosAx +
            2*(oldState ^. ball1Pos . _x - ball2PositionX  ) `savediv`
            norm (oldState ^. ball1Pos . _x - ball2PositionX) **2
        ballPosY <- decayIntegral 1  >>> decayIntegral 1 -< ball1PosAy +
            2*(oldState ^. ball1Pos . _y - ball2PositionY  ) `savediv`
            norm (oldState ^. ball1Pos . _y - ball2PositionY) **2
        ball1Position <- iPre 0 -< V2 ballPosX ballPosY
        let userSetting = ""
        returnA -< (State {
            _ball2Pos = ball2Position ,
            _ball1Pos = ball1Position,
            _userSetting = userSetting}, oldState)



    walk1D :: Process Stochastic () Double
    walk1D = proc _ -> do
        dacceleration <- constM (normal 0 8 ) -< ()
        acceleration <- decayIntegral 1 -< dacceleration
        returnA -< acceleration

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)


observationModel :: Process Stochastic State AgentObservation
observationModel = proc state -> do
    (n1, n2) <- noise &&& noise -< std
    returnA -< AgentObservation
                (state ^. ball1Pos + V2 n1 n2)
                (state ^. userSetting)

    where
        noise :: Process Stochastic Double Double
        noise = proc userInput -> do
            arrM (normal 0) -< userInput

posterior :: Process (Stochastic & Unnormalized) (AgentObservation, AgentInput) State
posterior = proc (obs, agentInput) -> do
  pred <- prior -< agentInput
  observe -<
    normalPdf (pred ^. ball1Pos . _x) std (obs ^. ball1Obs . _x)
    * normalPdf (pred ^. ball1Pos . _y) std (obs ^. ball1Obs . _y)
  arrM condition -< case obs ^. userObs of
    "up" ->    pred ^. ball1Pos . _y  > pred ^. ball2Pos . _y
    "down" ->  pred ^. ball1Pos . _y < pred ^. ball2Pos . _y
    "left" ->  pred ^. ball1Pos . _x < pred ^. ball2Pos . _x
    "right" -> pred ^. ball1Pos . _x > pred ^. ball2Pos . _x
    _ -> True
  observe -< case obs ^. userObs of
    "near center" -> normalPdf 0 0.1 (pred ^. ball1Pos . _x) * normalPdf 0 2 (pred ^. ball1Pos . _y)
    _ -> 1
  returnA -< set userSetting (obs ^. userObs) pred -- hardcode update for user input

control :: Process Stochastic Particles AgentInput
control = x

    where

    x = switch (withSwitch initial "go to the center") (const y)
    y = switch (withSwitch center "follow") (const x)


    initial = proc particles -> do
        let expectedX = V2
                (averageOf (fmap (first (view _x . _ball1Pos)) particles))
                (averageOf (fmap (first (view _y . _ball1Pos)) particles))
        let vel = expectedX + fmap (`savediv` (norm expectedX * 5)) expectedX
        returnA -< vel

    center = constM $ pure 0

    withSwitch a t = proc particles -> do
        out <- a -< particles
        sampledUserInput <- arrM empirical -< first (^. userSetting) <$> particles
        returnA -< (out, if sampledUserInput == t then Just out else Nothing )

empirical :: MonadSample m => [(a, Log Double)] -> m a
empirical population = do
    let (vs, ps) = unzip population
    i <- logCategorical $ V.fromList ps
    return $ vs !! i

mainSignal :: MVar T.Text -> Process (Stochastic & Feedback & ReadsStdIn) () (State, Particles, AgentObservation)
mainSignal mvar = proc () -> do
    rec
        previousUserInput <- iPre "" -< userInput
        (agentObs, state) <- system -< (agentInput, previousUserInput)
        (agentInput, particles) <- agent -< agentObs
        userInput <- user -< state

    returnA -< (state, particles, agentObs)

    where

    user :: Process (Stochastic & ReadsStdIn) UserObservation UserInput
    user = proc userObs -> do
        displayState -< userObs
        message <- readInput mvar -< ()
        returnA -< message

    system :: Process Stochastic (AgentInput, UserInput) (AgentObservation, UserObservation)
    system = proc (agentInput, userInput) -> do
        state <- prior -< agentInput
        observation <- observationModel -< set userSetting userInput state
        returnA -< (observation, state)

    agent :: Process (Stochastic & Feedback) AgentObservation (AgentInput, Particles)
    agent = proc agentObs -> do
        rec
            particles <- particleFilter 150 resampleMultinomial posterior -< (agentObs, agentInput)
            agentInput <- control -< particles
        returnA -< (agentInput, particles)

displayState :: Arrow a => a b b
displayState = returnA

readInput :: MonadIO m => MVar T.Text -> MSF m a T.Text
readInput mvar = constM (liftIO $ readMVar mvar)

gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ do
            mvar <- liftIO $ newMVar ""
            comm <- liftIO $ newMVar ""
            _ <- liftIO $ void $ forkIO $ forever do
                x <- T.getLine
                swapMVar mvar x
            reactimateCl Example.glossClock proc () -> do

                -- let noiseParser = ("set the noise level to " :: ParsecT Void T.Text Identity T.Text) >> float <* eof
                message <- constM (liftIO $ swapMVar mvar "") -< ()
                arrM (liftIO . swapMVar comm) -< message
                    -- "set the noise level to"
                showParticles <- hold False -< case message of
                    "show particles" -> Just True
                    "don't show particles" -> Just False
                    _ -> Nothing
                showObservations <- hold False -< case message of
                    "show observations" -> Just True
                    "don't show observations" -> Just False
                    _ -> Nothing
                (state, particles, measured) <- morphS (MM.hoist lift) (mainSignal comm) -< ()
                (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                    particles = if showParticles then particles else []
                                    , measured = if showObservations then measured else AgentObservation 100 ""
                                    , latent = state
                                    }

visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
visualisation = proc Result { particles, latent, measured} -> do

  drawBall -< (_ball2Pos latent, 0.1, blue)
  drawBall -< (_ball1Pos latent, 0.1, yellow)
  drawParticles -< first _ball1Pos <$> particles
  drawBall -< (measured ^. ball1Obs, 0.05, red)


drawBall :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color) ()
drawBall = proc (V2 x y, width, theColor) -> do
    arrMCl paintIO -<
        scale 150 150 $
        translate (into @Float x) (into @Float y) $
        color theColor $
        circleSolid $
        into @Float width

drawParticle :: MonadIO m => BehaviourF (GlossConcT m) td (V2 Double, Log Double) ()
drawParticle = proc (position, probability) -> do
  drawBall -< (position, 0.05, withAlpha (into @Float $ exp $ 0.2 * ln probability) violet)

drawParticles :: MonadIO m => BehaviourF (GlossConcT m) td [(V2 Double, Log Double)] ()
drawParticles = proc particles -> do
  case particles of
    [] -> returnA -< ()
    p : ps -> do
      drawParticle -< p
      drawParticles -< ps

data Result = Result
  {
   measured :: AgentObservation
  , latent :: State
  , particles :: [(State, Log Double)]
  }
  deriving Show

unangle' :: (Floating p, Ord p) => V2 p -> p
unangle' 0 = 0
unangle' x = unangle x







-- data Net = N { _nWeights1 :: L 20 1
--              , _nBias1    :: R 20
--              , _nWeights2 :: L  1  20
--              , _nBias2    :: R  1
--              }
--   deriving  Generic
--   deriving Show via Generically Net



-- instance Backprop Net

-- -- requires -XTemplateHaskell
-- makeLenses ''Net

-- minusNet n1 n2 = n1
--     & over nWeights1 (+ negate ((n2 ^. nWeights1)/ 100))
--     & over nWeights2 (+ negate ((n2 ^. nWeights2) / 100)) 
--     & over nBias1 (+ negate ((n2 ^. nBias1) / 100)) 
--     & over nBias2 (+ negate ((n2 ^. nBias2) / 100)) 

-- scaleNet (x) n2 = n2
--     & over nWeights1 ( undefined)
--     & over nWeights2 (+ (n2 ^. nWeights2)) 
--     & over nBias1 (+ (n2 ^. nBias1)) 
--     & over nBias2 (+ (n2 ^. nBias2)) 

-- runNet net x = z
--   where
--     -- run first layer
--     y = logistic $ (net ^^. nWeights1) #> x + (net ^^. nBias1)
--     -- run second layer
--     z = logistic $ (net ^^. nWeights2) #> y + (net ^^. nBias2)

-- logistic :: Floating a => a -> a
-- logistic x = 1 / (1 + exp (-x))


-- squaredError target output = error `VV.dot` error
--   where
--     error = target - output

-- netError target input net = squaredError (auto target)
--                                          (runNet net (auto input))

-- input = 0
-- output =  0

-- bar = evalBP (netError output input) net 

-- baz = gradBP (netError output input) net



-- net = (N {} 
--     & set nWeights1 0 
--     & set nWeights2 0 
--     & set nBias1 0 
--     & set nBias2 0 
--     )

-- -- trainStep
-- --       :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
-- --       => Double             -- ^ learning rate
-- --       -> R i                -- ^ input
-- --       -> R o                -- ^ target
-- --       -> Net   -- ^ initial Net
-- --       -> Net 
-- trainStep r !x !targ !n = n `minusNet` gradBP (netError x targ) n

-- -- > trainList
-- -- >     :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
-- -- >     => Double             -- ^ learning rate
-- -- >     -> [(R i, R o)]       -- ^ input and target pairs
-- -- >     -> Network i h1 h2 o  -- ^ initial network
-- -- >     -> Network i h1 h2 o
-- trainList r = flip $ foldl' (\n (x,y) -> trainStep r x y n)

-- m = sampleIO do
--     dat <- Control.Monad.replicateM 50 $ (\x -> (fromDouble x :: R 1 , fromDouble (sin x) :: R 1 )) <$> normal 0 1
--     let trainedNet = (Prelude.iterate (trainList 0 dat) net) !! 30
--     -- liftIO $ print 1
--     inp <- normal 0 1
--     liftIO $ print $  evalBP (netError (fromDouble $ sin inp) (fromDouble inp)) ( net)
--     liftIO $ print $  evalBP (netError (fromDouble $ sin inp) (fromDouble inp)) ( trainedNet)
--     -- print $ netError trainedNet


-- fromDouble x = A.fromList [x]

instance VectorSpace Angle Double where
    zeroVector = Angle 0
    Angle x ^+^ Angle y  = Angle $ x + y
    x *^ Angle y = Angle (x * y)
    Angle x `dot` Angle y = abs $ x - y

instance VectorSpace (V2 Double) Double where
    zeroVector = 0
    x ^+^  y  =  x + y
    x *^ y =  (x *) <$> y
    x `dot` y = x `V.dot` y
