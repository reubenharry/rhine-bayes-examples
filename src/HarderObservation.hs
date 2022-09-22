module HarderObservation where
import Control.Monad.Bayes.Class
import FRP.Rhine
import Example (Position, glossClock, Observation, visualisation, Result (particles, Result, measured, latent))
import qualified Data.Vector.Sized as V
import GHC.Float
import FRP.Rhine.Gloss
import Inference (pattern V2, onlineSMC, xCoord, yCoord)
import Control.Monad.Bayes.Sampler (sampleIO)
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Population (resampleMultinomial)

type Acceleration = V.Vector 2 Double

prior :: (MonadSample m, Diff td ~ Double) => BehaviourF m td () (Position, Acceleration)
prior = fmap (\((a,b), (c,d)) -> (V.fromTuple (a,c), V.fromTuple (b, d))) $ model1D &&& model1D where

    model1D = proc _ -> do
        acceleration <- constM (normal 0 4 ) >>> decayIntegral 2 -< ()
        velocity <- decayIntegral 2 -< acceleration -- Integral, dying off exponentially
        position <- decayIntegral 2 -< velocity
        returnA -< (position, acceleration)

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

generativeModel :: MonadSample m => BehaviourF m td (Position, Acceleration) (Observation, Observation)
generativeModel = proc (p, a) -> do
    
    n1 <- fmap V.fromTuple $ noise1 &&& noise1 -< ()
    n2 <- fmap V.fromTuple $ noise2 &&& noise2 -< ()
    returnA -< (p + n1, a + n2)

    where 
        noise1 = constM (normal 0 10)
        noise2 = constM (normal 0 1)


posterior :: (MonadInfer m, Diff td ~ Double) => BehaviourF m td (Observation, Observation) Position
posterior = proc ((V2 oPosX oPosY), (V2 oAccX oAccY)) -> do
  (pos@(V2 truePosX truePosY), acc@(V2 trueAccX trueAccY)) <- prior -< ()
  arrM factor -< normalPdf oAccY 1 trueAccY * normalPdf oAccX 1 trueAccX
  arrM factor -< 
    normalPdf oPosY 10 truePosY * 
    normalPdf oAccX 10 truePosX
  returnA -< pos

gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do
            lat@(actualPosition, _) <- prior -< ()
            pred@(measuredPosition, _) <- generativeModel -< lat
            samples <- onlineSMC 200 resampleMultinomial posterior -< pred
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = samples
                                , measured = measuredPosition
                                , latent = actualPosition
                                }
    