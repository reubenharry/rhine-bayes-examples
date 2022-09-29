{-# LANGUAGE TypeApplications #-}
module Loop where
import FRP.Rhine.Gloss
import Example hiding (posterior, generativeModel, prior)
import Control.Monad.Bayes.Sampler
import Inference
import Control.Monad.Bayes.Population
import Control.Monad.Morph
import Control.Monad.Bayes.Class
import qualified Data.Vector.Sized as V
import Data.Void
import Control.Monad.Trans.MSF ()
import Control.Monad.Trans.Reader


-- prior :: (MonadSample m, Diff td ~ Double) => BehaviourF m td () (V.Vector 2 Double)
-- sf :: ClSF (ExceptT e m) cl a b
-- sf :: MonadIO m => ClSF (ExceptT () m) cl a ()
-- sf = (runClSFExcept $ do
--      (safe count)
--      try $ throwS
--      safe count) >>> arrM (liftIO . print)


type SumClock = Millisecond 1

fillUp :: Monad m => ClSF (ExceptT Double m) SumClock Double ()
fillUp = proc x -> do
  s <- integral -< x
  _ <- throwOn' -< (s > 5, s)
  returnA       -< ()

helloWorld :: ClSFExcept IO SumClock () () Empty
helloWorld = do
  try $ arr (const 4) >>> fillUp
  once_ $ putStrLn "Hello World!"
  helloWorld

foo :: (MonadSample m, Diff (Time cl) ~ Double) => ClSFExcept m cl () Position Empty
foo = do
    try $ abortivePrior
    foo

abortivePrior :: (MonadSample m, Diff (Time cl) ~ Double) => ClSF (ExceptT Position m) cl () Position
abortivePrior = proc () -> do
    x <- prior -< ()
    _ <- throwOn' -< (norm x > 2, x)
    returnA -< x


prior :: StochasticSignal Position
prior = fmap V.fromTuple $ model1D &&& model1D where

    model1D = proc _ -> do
        dacceleration <- constM (normal 0 8 ) -< ()
        acceleration <- integral -< dacceleration
        velocity <- integral -< acceleration -- Integral, dying off exponentially
        position <- integral -< velocity
        returnA -< position

generativeModel :: StochasticSignalTransform Position Observation
generativeModel = proc p -> do
    n <- fmap V.fromTuple $ noise &&& noise -< ()
    returnA -< p + n

    where 
        noise = constM (normal 0 std)


posterior ::StochasticSignalTransformUnnormalized Observation Position
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- man -< ()
  arrM factor -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

man :: (MonadSample m, Diff (Time cl) ~ Double) => ClSF m cl () Position
man = safely foo 
-- >>> arrM (liftIO . print)
-- run = runExceptT $ reactimateCl (waitClock @100) sf

-- man2 :: IO ()
-- man2 = reactimateCl (waitClock @100) $ morphS (Control.Monad.Morph.hoist sampleIO) man

-- prior = undefined
    -- fmap V.fromTuple $ model1D &&& model1D where

    -- model1D = proc _ -> do
    --     dacceleration <- constM (normal 0 8 ) -< ()
    --     acceleration <- decayIntegral 1 -< dacceleration
    --     velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
    --     position <- decayIntegral 1 -< velocity
    --     returnA -< position

    -- decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl Example.glossClock proc () -> do
            actualPosition <- man -< ()
            measuredPosition <- generativeModel -< actualPosition
            samples <- onlineSMC 200 resampleMultinomial posterior -< measuredPosition
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = samples
                                , measured = measuredPosition
                                , latent = actualPosition
                                }