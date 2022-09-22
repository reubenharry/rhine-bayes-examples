{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}


module RMSMC where
import Control.Monad.Bayes.Population
    ( resampleMultinomial )
import Data.MonadicStreamFunction
    ( returnA, (>>>), arrM, constM, Arrow(first, arr, (&&&)), withSideEffect_ )
import Control.Monad.Bayes.Class
    ( MonadSample(normal, bernoulli, random), factor, normalPdf, MonadInfer )
import FRP.Rhine
    ( VectorSpace((*^)),
      arrMCl,
      average,
      liftClock,
      reactimateCl,
      LiftClock, TimeDomain (Diff), BehaviourF, RescaledClock (RescaledClock, unscaledClock, rescale), MonadIO )
import GHC.Float (float2Double, double2Float)
import FRP.Rhine.Gloss
    ( blue,
      green,
      red,
      withAlpha,
      circleSolid,
      color,
      scale,
      translate,
      Display(InWindow),
      defaultSettings,
      clearIO,
      launchGlossThread,
      paintIO,
      GlossSettings(display),
      GlossConcT,
      GlossSimClockIO(..), Event (EventKey), Key (Char), KeyState (Down), violet, Color )
import Control.Monad.Bayes.Sampler ( sampleIO, SamplerIO )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import qualified Data.Vector.Sized as V
import Numeric.Hamilton ()
import Numeric.LinearAlgebra.Static ()
import Control.Monad.Trans.Identity ( IdentityT(runIdentityT) )
import Inference (pattern V2, V2, xCoord, yCoord, NormalizedDistribution, StochasticSignal, StochasticSignalTransform, UnnormalizedDistribution, onlineSMC, onlineRMSMC)
import Numeric.Log
import qualified Control.Category as C


std :: Double
std = 0.5

type Observation = V.Vector 2 Double
type Position = V.Vector 2 Double


prior :: (MonadSample m, Diff td ~ Double) => BehaviourF m td () Position
prior = fmap V.fromTuple $ model1D &&& model1D where

    model1D = proc _ -> do
        acceleration <- constM (normal 0 4 ) -< ()
        velocity <- decayIntegral 2 -< acceleration -- Integral, dying off exponentially
        position <- decayIntegral 2 -< velocity
        returnA -< position

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

generativeModel :: (MonadSample m, Diff td ~ Double) => BehaviourF m td Position Observation
generativeModel = proc p -> do
    n <- fmap V.fromTuple $ noise &&& noise -< ()
    -- isOutlier <- constM (bernoulli 0.1) -< ()
    returnA -< p + n
    -- if isOutlier then fmap V.fromTuple $ outlier &&& outlier-< () else returnA -< p + n 

    where 
        noise = constM (normal 0 std)
        -- outlier = constM ((\x -> 10 * (x - 0.5)) <$> random)


posterior :: (MonadInfer m, Diff td ~ Double) => BehaviourF m td Observation Position
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- prior -< () -- fmap V.fromTuple $ (constM ((\x -> 10 * (x - 0.5)) <$> random)) &&& (constM ((\x -> 10 * (x - 0.5)) <$> random)) -< ()
--   observation <- generativeModel -< latent
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
            samples <- onlineRMSMC 1 resampleMultinomial posterior -< measuredPosition
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
  drawBall -< (position, 0.1, violet)

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




