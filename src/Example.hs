{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}


module Example where

import qualified Data.Vector.Sized as V
import FRP.Rhine
import Inference (pattern V2, V2, onlineSMC, StochasticSignal, NormalizedDistribution, StochasticSignalTransform, StochasticSignalTransformUnnormalized)
import FRP.Rhine.Gloss
import Numeric.Log
import GHC.Float
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Population
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Class

std :: Double
std = 0.5

type Observation = V.Vector 2 Double
type Position = V.Vector 2 Double


prior :: StochasticSignal Position
prior = fmap V.fromTuple $ model1D &&& model1D where

    model1D = proc _ -> do
        dacceleration <- constM (normal 0 8 ) -< ()
        acceleration <- decayIntegral 1 -< dacceleration
        velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
        position <- decayIntegral 1 -< velocity
        returnA -< position

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

generativeModel :: StochasticSignalTransform Position Observation
generativeModel = proc p -> do
    n <- fmap V.fromTuple $ noise &&& noise -< ()
    returnA -< p + n

    where 
        noise = constM (normal 0 std)


posterior ::StochasticSignalTransformUnnormalized Observation Position
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- prior -< ()
  arrM factor -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent








----------
-- display
----------

gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do
            actualPosition <- prior -< ()
            measuredPosition <- generativeModel -< actualPosition
            samples <- onlineSMC 200 resampleMultinomial posterior -< measuredPosition
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = samples
                                , measured = measuredPosition
                                , latent = actualPosition
                                }










dot :: IO ()
dot = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do
            actualPosition <- prior -< ()
            measuredPosition <- generativeModel -< actualPosition
            samples <- onlineSMC 200 resampleMultinomial posterior -< measuredPosition
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = []
                                , measured = measuredPosition
                                , latent = actualPosition
                                }


noObservations :: IO ()
noObservations = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do
            actualPosition <- prior -< ()
            measuredPosition <- generativeModel -< actualPosition
            samples <- onlineSMC 200 resampleMultinomial posterior2 -< measuredPosition
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = samples
                                , measured = measuredPosition
                                , latent = actualPosition
                                }

weakPrior :: IO ()
weakPrior = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do
            actualPosition <- prior -< ()
            measuredPosition <- generativeModel -< actualPosition
            samples <- onlineSMC 200 resampleMultinomial posterior3 -< measuredPosition
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = samples
                                , measured = measuredPosition
                                , latent = actualPosition
                                }





visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
visualisation = proc Result { particles, measured, latent} -> do

  drawParticles -< particles
  drawBall -< (measured, 0.05, red)
  drawBall -< (latent, 0.3, withAlpha 0.5 green)

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



glossClock :: RescaledClock GlossSimClockIO Double
glossClock = RescaledClock
  { unscaledClock = GlossSimClockIO
  , rescale = float2Double
  }


posterior2 :: (MonadInfer m, Diff td ~ Double) => BehaviourF m td Observation Position
posterior2 = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- prior -< () -- fmap V.fromTuple $ (constM ((\x -> 10 * (x - 0.5)) <$> random)) &&& (constM ((\x -> 10 * (x - 0.5)) <$> random)) -< ()
--   arrM factor -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

posterior3 :: (MonadInfer m, Diff td ~ Double) => BehaviourF m td Observation Position
posterior3 = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- fmap V.fromTuple $ (constM ((\x -> 10 * (x - 0.5)) <$> random)) &&& (constM ((\x -> 10 * (x - 0.5)) <$> random)) -< ()
  arrM factor -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent



    -- isOutlier <- constM (bernoulli 0.1) -< ()
    -- if isOutlier then fmap V.fromTuple $ outlier &&& outlier-< () else returnA -< p + n 
        -- outlier = constM ((\x -> 10 * (x - 0.5)) <$> random)
