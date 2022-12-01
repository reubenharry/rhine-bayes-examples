module Future where

import qualified Data.Vector.Sized as V
import Control.Monad.Bayes.Class
import FRP.Rhine
import Inference (pattern V2, V2, particleFilter, observe)
import FRP.Rhine.Gloss hiding (shift)
import Numeric.Log
import GHC.Float
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Population
import Control.Monad.Trans.Class
import Example 
import Control.Monad.Trans.MSF (ReaderT)

-- futurePosterior :: (MonadInfer m, Diff td ~ Double, TimeDomain td) => BehaviourF m td Observation Position
futurePosterior :: MSF
  (ReaderT
     (TimeInfo (RescaledClock GlossSimClockIO Double))
     (Population (GlossConcT SamplerIO)))
  (V2 Double)
  Position
futurePosterior = proc (V2 oX oY) -> do
  latent <- prior -< ()
  shifted@(V2 trueX trueY) <- delayBy 15 -< latent
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent


future :: IO ()
future = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do
            actualPosition <- prior -< ()
            thePast <- delayBy 15 -< actualPosition
            measuredPosition <- observationModel -< thePast
            samples <- particleFilter 200 resampleMultinomial futurePosterior -< measuredPosition
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = samples
                                , measured = thePast
                                , latent = actualPosition
                                }

past :: IO ()
past = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do
            actualPosition <- prior -< ()
            thePast <- delayBy 50 -< actualPosition
            measuredPosition <- observationModel -< actualPosition
            samples <- particleFilter 200 resampleMultinomial (posterior >>> delayBy 50) -< measuredPosition
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = samples
                                , measured = thePast
                                , latent = actualPosition
                                }

pastFilter :: IO ()
pastFilter = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do
            actualPosition <- prior -< ()
            thePast <- delayBy 50 -< actualPosition
            measuredPosition <- observationModel -< actualPosition
            samples <- (particleFilter 200 resampleMultinomial posterior >>> delayBy 50) -< measuredPosition
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = samples
                                , measured = thePast
                                , latent = actualPosition
                                }

shift :: Monad m => Int -> MSF m c c
shift n = accumulateWith (\x xs -> take n $ x : xs) [] >>> arr last