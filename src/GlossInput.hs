{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}


module GlossInput where

import qualified Data.Vector.Sized as V
import FRP.Rhine
import Inference (pattern V2, V2, particleFilter, StochasticSignal, StochasticSignalTransform, StochasticSignalTransformUnnormalized, hold, observe)
import FRP.Rhine.Gloss
import Numeric.Log
import GHC.Float
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Population
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator (enumerate)
import qualified Data.Vector as VV
import Data.List (intersperse)
import qualified Control.Monad.Morph as MM
import Witch (into)
import Control.Monad.Fix (MonadFix)
import Debug.Trace (traceM)
import qualified Debug.Trace as Debug
import Data.MonadicStreamFunction.InternalCore (MSF(..))

std :: Double
std = 0.3

type Observation = V.Vector 2 Double
type Position = V.Vector 2 Double


prior :: StochasticSignal Position
prior = proc _ -> do
  x <- walk1D -< ()
  y <- walk1D -< ()
  returnA -< V.fromTuple (x, y)



walk1D :: StochasticSignalTransform () Double
walk1D = proc _ -> do
    dacceleration <- constM (normal 0 8 ) -< ()
    acceleration <- decayIntegral 1 -< dacceleration
    velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
    position <- decayIntegral 1 -< velocity
    -- time <- count -< ()
    returnA -< position --  + 0.25 * sin (time/2)

  where
    decayIntegral timeConstant =  av >>> arr (timeConstant *^)

av :: (Monad m, VectorSpace v a0, Num v, Fractional v) => MSF
  m
  v
  v
av = proc v -> do 
    vs <- accumulateWith (\x y ->  take 100 (x : y)) [] -< v
    returnA -< (* (1/100)) $ foldr1 (^+^) $ vs


observationModel :: StochasticSignalTransform Position Observation
observationModel = proc p -> do
    n <- fmap V.fromTuple $ noise &&& noise -< ()
    returnA -< p + n

    where
        noise = constM (normal 0 std)


posterior ::StochasticSignalTransformUnnormalized Observation Position
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- prior -< ()
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

posteriorPredictive ::StochasticSignalTransformUnnormalized Observation (Position, Int)
posteriorPredictive = proc obs -> do
  predicted <- posterior -< obs
  e <- edgeBy ((>0.5) . norm) -< predicted
  returnA -< (predicted, e)







----------
-- display
----------

gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        do 
          
          (clock, _) <- initClock glossClock
          reactimateCl glossClock proc () -> do
            -- actualPosition <- prior -< ()
            (_, tag) <- morphS lift clock -< ()
            actualPosition@(V2 x y) <- arr (\case 
                EventMotion (x,y) -> Just (into @Double x, into @Double y)
                _ -> Nothing ) >>> hold (0,0) >>> arr (\(x,y) -> V2 (x/150) (y/150)) -< tag
            -- arrMCl paintIO -< translate (into @Float x) (into @Float y) $ circle 15
            measuredPosition <- observationModel -< actualPosition
            samples <- particleFilter 50 resampleMultinomial posterior -< measuredPosition
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = samples
                                , measured = measuredPosition
                                , latent =  actualPosition
                                }


empirical :: MonadSample m => [(b, Log Double)] -> m b
empirical ls = do
  let (vs, ps) = unzip ls
  i <- logCategorical $ VV.fromList ps
  return (vs !! i)

edge :: Monad m => MSF m Bool Int
edge = feedback 0 proc (b, prev) -> do
  oldB <- iPre True -< b
  returnA -< (prev + if oldB==b then 0 else 1, prev + if oldB==b then 0 else 1)

edgeBy :: Monad m => (a -> Bool) -> MSF m a Int
edgeBy p = proc a -> do
  b <- arr p -< a
  e <- edge -< b
  returnA -< e

rep :: IO ()
rep = reactimate $ proc () -> do
  b <- constM (read @Integer <$> getLine) -< ()
  c <- edgeBy even -< b
  arrM print -< c

visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
visualisation = proc Result { particles, measured, latent} -> do

  drawBall -< (measured, 0.05, red)
  drawBall -< (latent, 0.3, makeColorI 255 239 0 255)
  drawParticles -< particles

visualisation' :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td [String] ()
visualisation' = proc str -> do

  arrMCl paintIO -< translate 0 200 $ scale 0.1 0.1 $ pictures $ [translate 0 (height * 100) $ text l | (l, height) <- zip (reverse str) [1..]]
  arrMCl paintIO -< circle 75

drawBall :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color) ()
drawBall = proc (V2 x y, width, theColor) -> do
    arrMCl paintIO -<
        scale 150 150 $
        translate (double2Float x) (double2Float y) $
        color theColor $
        circleSolid $
        double2Float width

drawParticle :: MonadIO m => BehaviourF (GlossConcT m) td (Position, Log Double) ()
drawParticle = proc (position, probability) -> do
  drawBall -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) violet)

drawParticles :: MonadIO m => BehaviourF (GlossConcT m) td [(Position, Log Double)] ()
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
  , latent :: Position
  , particles :: [(Position, Log Double)]
  }
  deriving Show

-- glossClock :: LiftClock (GlossConcT SamplerIO) IdentityT GlossSimClockIO
-- glossClock = liftClock GlossSimClockIO



glossClock :: RescaledClock GlossEventClockIO Double
glossClock = RescaledClock
  { unscaledClock = GlossEventClockIO
  , rescale = float2Double
  }


posterior2 :: (MonadInfer m, Diff td ~ Double) => BehaviourF m td Observation Position
posterior2 = proc (V2 _ _) -> do
  latent@(V2 _ _) <- prior -< () -- fmap V.fromTuple $ (constM ((\x -> 10 * (x - 0.5)) <$> random)) &&& (constM ((\x -> 10 * (x - 0.5)) <$> random)) -< ()
--   observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

posterior3 :: (MonadInfer m, Diff td ~ Double) => BehaviourF m td Observation Position
posterior3 = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- fmap V.fromTuple $ constM ((\x -> 10 * (x - 0.5)) <$> random) &&& constM ((\x -> 10 * (x - 0.5)) <$> random) -< ()
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent



    -- isOutlier <- constM (bernoulli 0.1) -< ()
    -- if isOutlier then fmap V.fromTuple $ outlier &&& outlier-< () else returnA -< p + n 
        -- outlier = constM ((\x -> 10 * (x - 0.5)) <$> random)
