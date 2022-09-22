{-# LANGUAGE TypeApplications #-}
module Physics where

import Control.Monad.Bayes.Population
    ( fromWeightedList,
      normalize,
      resampleMultinomial,
      runPopulation,
      spawn,
      Population )
import Data.MonadicStreamFunction
    ( returnA, (>>>), arrM, constM, Arrow(first, arr, (&&&)), MSF, reactimate, liftBaseM, liftBaseS, liftTransS, feedback, withSideEffect_ )
import Numeric.Log ( Log(ln) )
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import Data.Functor (($>))
import Control.Monad.Bayes.Class
    ( MonadSample(normal, uniformD, random), factor, normalPdf, MonadInfer )
import FRP.Rhine
    ( VectorSpace((^/), (^+^), zeroVector, (*^)),
      arrMCl,
      constMCl,
      average,
      liftClock,
      reactimateCl,
      ClSF,
      LiftClock, integral, MonadIO (liftIO), RunningClockInit, absoluteS, liftClSF, hoistClSF, flow, Rhine (Rhine), SN (Synchronous), clId, (@@), TimeDomain(..), UTCTime, RescaledClock (RescaledClock, rescale, unscaledClock), Clock (Time), collect, (>--), (-->), concurrently, Millisecond, waitClock, RescaledClockM (RescaledClockM), SequentialClock (SequentialClock, sequentialCl1, sequentialCl2, sequentialSchedule), Schedule (Schedule), GetClockProxy, In, Out, ParallelClock (ParallelClock, parallelCl1, parallelCl2, parallelSchedule), BehaviourF, sinceLastS )
import GHC.Float (float2Double, double2Float, showFloat)
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
      initClock,
      GlossSettings(display),
      GlossConcT,
      GlossSimClockIO(..), GlossEventClockIO (GlossEventClockIO), Event (EventKey), text, Key (Char), KeyState (Down), flowGloss )
import Control.Monad.Bayes.Sampler ( sampleIO, SamplerIO )
import qualified Control.Monad.Trans.MSF as DunaiReader
import Data.Tuple ( swap )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import qualified Data.Vector.Sized as V
import Numeric.Hamilton (System, mkSystem', Phase (Phs), stepHam, fromPhase, underlyingPos, Config (cfgPositions))
import Numeric.LinearAlgebra.Static (vec2, Sized (extract))
import Control.Monad.Trans.Identity ( IdentityT(runIdentityT) )
import Inference (pattern V2, pattern V1, xCoord, yCoord, NormalizedDistribution, StochasticSignal, StochasticSignalTransform, UnnormalizedDistribution, onlineSMC)
import Control.Applicative
import Control.Monad.Trans.MSF (runReader, runReaderS, readerS)
import Data.Functor.Identity
import Example (Result(..), glossClock, visualisation, Observation, Position)
import qualified Data.Vector.Storable as V
import TwoStream (liftMSF)



s :: (forall a . RealFloat a => a) -> System 1 1
s n = mkSystem' 1   -- masses
    id
    (\(V1 x) -> ((n :: forall a . RealFloat a => a)/10) * (x**2))
                --   (\(V1 θ)   -> V2 (sin θ) (0.5 - cos θ))     -- coordinates
                --   (\(V2 _ y) -> y                       )     -- potential


-- s :: System 2 1
-- s = mkSystem' (vec2 1 1                             )     -- masses
--                   (\(V1 θ)   -> V2 (sin θ) (0.5 - cos θ))     -- coordinates
--                   (\(V2 _ y) -> y                       )     -- potential


-- s2 :: System 2 1
-- s2 = mkSystem' (vec2 1 1                             )     -- masses
--                   (\(V1 θ)   -> V2 (sin θ * 2) (0.5 - cos θ * 2))     -- coordinates
--                   (\(V2 _ y) -> y                       )     -- potential

foo = showFloat  

system :: (Monad m, MonadSample m) => MSF m (System 1 1) (System 1 1)
system = liftMSF do
    x <- (100*) <$> random
    return (constM (pure (s $ (encodeFloat (floor x) 2))))

-- (uniformD [constM (pure $ s 1), constM (pure $ s 2)])

prior :: (MonadSample m, Diff td ~ Double) => BehaviourF m td () (Phase 1)
prior = feedback (Phs 1 1) $ proc (_, phasePoint) -> do
  time <- sinceLastS -< ()
  s <- system -< s 1
  let next = stepHam time s phasePoint
  returnA -< (next, next)

generativeModel :: (MonadSample m, Diff td ~ Double) => BehaviourF m td (Phase 1) Observation
generativeModel = proc p -> do
    -- n <- fmap V.fromTuple $ noise &&& noise -< ()
    -- isOutlier <- constM (bernoulli 0.1) -< ()
    returnA -< getPos p
    -- if isOutlier then fmap V.fromTuple $ outlier &&& outlier-< () else returnA -< p + n 

    -- where 
    --     noise = constM (normal 0 std)

std :: Double
std = 2
        -- outlier = constM ((\x -> 10 * (x - 0.5)) <$> random)


posterior :: (MonadInfer m, Diff td ~ Double) => BehaviourF m td Observation (Phase 1)
posterior = proc (V2 oX oY) -> do
  latent <- prior -< () 
  obs@(V2 trueX trueY) <- generativeModel -< latent -- fmap V.fromTuple $ (constM ((\x -> 10 * (x - 0.5)) <$> random)) &&& (constM ((\x -> 10 * (x - 0.5)) <$> random)) -< ()
  arrM factor -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

getPos pos = (\x -> V.fromTuple (0,x)) $ (V.! 0) $ extract $ underlyingPos (s 0) $ cfgPositions $ fromPhase (s 0) pos

gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do
            actualPosition <- prior -< ()
            measuredPosition <- generativeModel -< actualPosition
            n <- fmap V.fromTuple $ noise &&& noise -< ()
            samples <- onlineSMC 100 resampleMultinomial posterior -< (measuredPosition + n)
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = first getPos <$> samples
                                , measured = measuredPosition + n
                                , latent = getPos actualPosition
                                }
            where noise = constM (normal 0 std)

--   let newPoint2 = stepHam (float2Double time * 2) s2 phasePoint

--   -- choice <- arrMCl uniformD -< [newPoint, newPoint2]
--   returnA -< (newPoint, newPoint)

-- foo :: Monad m => m (MSF m a b)
-- foo = undefined

-- bar :: Monad m => m (MSF m a a) -> MSF m a a
-- bar msf = MSF \x -> liftA2 (,) (pure x) msf

-- baz :: Monad m => MSF m a Double
-- baz = constM (pure 5)

-- baz' :: Monad m => MSF m a Double
-- baz' = constM (pure 1)

-- blab :: MonadSample m => MSF m Double Double
-- blab = bar (uniformD [baz, baz'])

-- -- run :: IO ()
-- -- run = sampleIO $ runIdentityT $ reactimate $ 
-- --     (constM (pure 0) >>> runPopulationS 1000 resampleMultinomial blab >>> arrM (liftIO . print . averageOf))

-- --  foo = reactimateCl (waitClock @100) (morphS undefined prior >>> arrM (print))

-- -- instance KnownNat n => Clock SamplerIO (Millisecond n) where
-- --     -- type Time ((Millisecond n)) = Time ( (Millisecond n))
-- --     initClock m = (liftIO :: forall a. IO a -> SamplerIO a) $ first (morphS liftIO :: RunningClock IO UTCTime Bool
-- --         -> RunningClock SamplerIO UTCTime Bool) <$> initClock @IO m


-- ex :: IO ()
-- ex = flow ((msf @@ sclock))

-- reactimateCl' :: forall (m :: * -> *) cl.
--     (Monad m, Clock m cl, GetClockProxy cl, cl ~ In cl, cl ~ Out cl) =>
--     cl -> ClSF m cl () () -> m ()
-- reactimateCl' = undefined

-- -- msf :: ClSF IO (SequentialClock IO (Millisecond 100) (Millisecond 100)) () ()
-- msf = constM (liftIO (print "ping"))

-- -- clock :: RescaledClockM
-- --   IO (Millisecond 1000) UTCTime
-- -- clock = RescaledClockM (waitClock @1000) ((\x -> pure x))

-- sclock :: ParallelClock IO (Millisecond 3000) (Millisecond 4000)
-- sclock = ParallelClock {
--     parallelCl1 = waitClock,	 
--     parallelCl2 = waitClock,
--     parallelSchedule = concurrently
--     }

-- console :: ClSF IO (RescaledClockFloat (Millisecond 2000)) ModelState ()
-- console = arrMCl consoleOutput

-- ex = 
--     clId               @@  rescaleClockFloat StdinCloc
--     >-- collect            -@- concurrently
--     --> game               @@  rescaleClockFloat waitClock
--     >-- keepLast undefined -@- concurrently
--     --> console            @@  rescaleClockFloat waitClock
-- ex = flow (Rhine (Synchronous (constM @IO $ pure ())) (undefined))


-- | A newtype for 'UTCTime' with 'Float' instead of 'Double' as time difference.
-- newtype UTCTimeFloat = UTCTimeFloat UTCTime

-- instance TimeDomain UTCTimeFloat where
--   type Diff (UTCTimeFloat) = Float
--   diffTime (UTCTimeFloat t1) (UTCTimeFloat t2) = realToFrac $ diffTime t1 t2

-- -- | A clock rescaled to the 'TimeDomain' 'UTCTimeFloat'.
-- type RescaledClockFloat cl = RescaledClock cl UTCTimeFloat

-- -- | The rescaled clock value.
-- rescaleClockFloat :: Time cl ~ UTCTime => cl -> RescaledClockFloat cl
-- rescaleClockFloat cl = RescaledClock
--   { rescale       = UTCTimeFloat
--   , unscaledClock = cl
--   }



