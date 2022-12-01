{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Pong where
import FRP.Rhine.Gloss hiding (Up, Down)
import Prelude hiding (until)
import qualified Example
import qualified Control.Monad.Morph as MM
import Linear.V2 (V2(..), _x, _y)
import Control.Monad.Bayes.Class
import Numeric.Log
import Control.Monad.Bayes.Sampler
import Control.Monad.Morph
import Inference hiding (V2)
import Control.Monad.Bayes.Population (resampleMultinomial)
import Active (averageOf)
import Witch (into)
import Control.Lens
import Control.Monad (forever)
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.MSF (untilE)
import qualified Control.Category as C
import Data.Maybe (isJust, fromMaybe)
import Control.Monad.Trans.MSF.Reader (ReaderT)
import Text.Read (readMaybe)
import Data.Functor (void)
import Control.Concurrent (swapMVar, readMVar)
import Control.Concurrent.MVar
import qualified Data.Text as T
import Graphics.Gloss (loadBMP)
import Text.Megaparsec (Parsec, MonadParsec (eof), runParser)
import Data.Void (Void)
import Control.Applicative (optional)
import Text.Megaparsec.Char (digitChar)
import Data.Char (digitToInt)
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Either (isRight)


-- bouncing

std :: Double
std = 0.3


data State = State {
    _ballPos :: V2 Double,
    _barPos :: Double
    } deriving Show

-- a simple macro, not important
$(makeLenses ''State)


type Observation = V2 Double
type Action = Double
type Particles = [(State, Log Double)]
data Direction = Up | Down


-- "Process Deterministic x y" is the type of signal functions 
-- where the input signal is (Time -> x)
-- and the output signal is (Time -> y)
-- So: (Time -> x) -> (Time -> y)
-- THIS IS NOT THE SAME AS A TIME-VARYING FUNCTION!! Time -> (x -> y)
-- "Process Stochastic x y" then is (effectively): (Time -> x) -> Distribution (Time -> y)



prior :: Process Stochastic Action State
prior = safely $ S.evalStateT loop (State (V2 0 (-2)) 0) where

    loop = forever do
        initialPosition <- S.get
        stateAtTimeofUpCollision <- until collisionUp (movement initialPosition Up)
        stateAtTimeofDownCollision <- until collisionDown (movement stateAtTimeofUpCollision Down)
        S.put stateAtTimeofDownCollision


    collisionUp (State (V2 x y) bar) = y > 2 || (y < 0 && y > -0.1) && abs (x - bar) < 0.2
    collisionDown ((State (V2 x y) bar)) = negate y > 2 || (y > 0 && y < 0.1) && abs (x - bar) < 0.2

    movement :: State -> Direction -> Process Stochastic Action State
    movement state dir = proc action -> do
        barPosition <- iPre (state ^. barPos) -< action
        ballPosX <- walk1D -< ()
        ballPosY <- (case dir of Up -> id; Down -> negate) <$> time -< ()
        returnA -< (state
            & set barPos barPosition
            & over ballPos (+ V2 ballPosX ballPosY) )



    walk1D :: Process Stochastic () Double
    walk1D = proc _ -> do
        dacceleration <- constM (normal 0 8 ) -< ()
        acceleration <- decayIntegral 1 -< dacceleration
        velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
        position <- decayIntegral 1 -< velocity
        returnA -< position

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

until cond signal = lift $ try proc input -> do
    output <- signal -< input
    _ <- throwOn' -< (cond output, output)
    returnA -< output

observationModel :: Process Stochastic State Observation
observationModel = proc state -> do
    (n1, n2) <- noise &&& noise -< ()
    returnA -< state ^. ballPos + V2 n1 n2

    where
        noise :: Process Stochastic () Double
        noise = constM (normal 0 std)


posterior :: Process (Stochastic & Unnormalized) (Observation, Action) State
posterior = proc (V2 oX oY, action) -> do
  pred <- prior -< action
  observe -< normalPdf (pred ^. ballPos . _x) std oX  * normalPdf (pred ^. ballPos . _y) std oY
  returnA -< pred

control :: Process Stochastic Particles Action
control = arr (averageOf . map (first (view _x . _ballPos)))



mainSignal :: Process (Stochastic & Feedback) () (State, Particles)
mainSignal = proc _ -> do
    rec
        -- n <- count -< ()
        -- let action = (sin (n/50))
        -- let action = 0
        state <- prior -< action -- the ground truth state (it's drawn from the prior)
        observations <- observationModel -< state
        particles <- particleFilter 150 resampleMultinomial posterior -< (observations, action)
        action <- control -< particles

    returnA -< (state, particles)




gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl Example.glossClock proc () -> do

            (state, particles) <- morphS (MM.hoist lift) mainSignal -< ()
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = particles
                                , measured = 0
                                , latent = state
                                , bar = state ^. barPos
                                }

visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
visualisation = proc Result { particles, latent, bar} -> do

  drawParticles -< first _ballPos <$> particles
--   Example.drawBall -< (measured, 0.05, red)
  drawBall -< (_ballPos latent, 0.1, withAlpha 0.5 green)

  arrMCl paintIO -< translate (into @Float (bar * 150)) 0 $ polygon [(-30, -5), (-30, 5), (30, 5), (30, -5), (-30, -5)]

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
  drawBall -< (position, 0.1, withAlpha (into @Float $ exp $ 0.2 * ln probability) violet)

drawParticles :: MonadIO m => BehaviourF (GlossConcT m) td [(V2 Double, Log Double)] ()
drawParticles = proc particles -> do
  case particles of
    [] -> returnA -< ()
    p : ps -> do
      drawParticle -< p
      drawParticles -< ps

data Result = Result
  {
    --   estimate :: Position
    -- stdDev :: Double
   measured :: Observation
  , latent :: State
  , particles :: [(State, Log Double)]
  , bar :: Double
  }
  deriving Show



--  proc y -> do
--     x <- arr (averageOf . fmap (first ((\(V2 x y) -> x) . _ballPos))) -< y
--     n <- count -< ()
--     -- x' <- arr (sin) -< (n/10)
--     returnA -< x -- (3*x') + x
