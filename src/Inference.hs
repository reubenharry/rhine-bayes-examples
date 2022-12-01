{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Inference where
import Control.Monad.Bayes.Population
import Data.MonadicStreamFunction
import Numeric.Log
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import Data.Functor (($>))
import Control.Monad.Bayes.Class
import FRP.Rhine hiding (normalize)
import FRP.Rhine.Gloss hiding (normalize)
import Control.Monad.Bayes.Sampler
import qualified Control.Monad.Trans.MSF as DunaiReader
import Control.Monad.Trans.Class
import qualified Data.Vector.Sized as V
import Control.Monad.Trans.Identity
import Data.Monoid (Endo (appEndo, Endo))
import qualified Control.Monad.Bayes.Traced.Static as TrStat
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.Trans.MSF (ReaderT)
import Data.Kind (Constraint)
import GHC.Base (Type)


-- todos


-- the splitting particle:
-- real time commands:
  -- track the position of the robot
-- enumeratorT or analyticT for integrable observation models
-- pmmh for static variables:
    -- particle mass
    -- respawning?
-- Poisson process
-- recurrent switching lds  
-- bearing example
-- user input
-- two streams: one is a boolean and when true, means dot is in top half
-- language

hold :: Monad m => a -> MSF m (Maybe a) a
hold a = feedback a proc (x, old) -> do
    case x of
        Just y -> returnA -< (y, y)
        Nothing -> returnA -< (old, old)



instance MonadFix SamplerIO where
    mfix f = liftIO (mfix (sampleIO . f))

instance MonadFix m => MonadFix (Population m) where
    -- mfix f = undefined $ \x -> runPopulation $ f x


instance MonadSample m => MonadSample (GlossConcT m) where
  random = lift random

instance MonadSample m => MonadSample (ExceptT e m) where
  random = lift random

glossClock :: LiftClock (GlossConcT SamplerIO) IdentityT GlossSimClockIO
glossClock = liftClock GlossSimClockIO


particleFilter :: forall m cl a b . Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x . Population m x -> Population m x)
  -> ClSF (Population m) cl a b
  -> ClSF m cl a [(b, Log Double)]
particleFilter nParticles resampler = withReaderS $ particleFilter' nParticles resampler


withReaderS :: (Monad m1, Monad m2) =>
  (MSF m2 (r1, a1) b1 -> MSF m1 (r2, a2) b2)
  -> MSF (DunaiReader.ReaderT r1 m2) a1 b1
  -> MSF (DunaiReader.ReaderT r2 m1) a2 b2
withReaderS f = DunaiReader.readerS . f . DunaiReader.runReaderS

particleFilter' :: forall m a b . Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x . Population m x -> Population m x)
  -> MSF (Population m) a b
  -> MSF m a [(b, Log Double)]
particleFilter' nParticles resampler msf = particleFilter'' $ spawn nParticles $> msf
  where
    particleFilter'' :: Population m (MSF (Population m) a b) -> MSF m a [(b, Log Double)]
    particleFilter'' msfs = MSF $ \a -> do
      -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
      bAndMSFs <- runPopulation $ normalize $ resampler $ flip unMSF a =<< msfs
      -- FIXME This abominal lambda could be done away by using Weighted?
      let (currentPopulation, continuations) = unzip $ (\((b, msf), weight) -> ((b, weight), (msf, weight))) <$> bAndMSFs
      -- FIXME This normalizes, which introduces bias, whatever that means
      return (currentPopulation, particleFilter'' $ fromWeightedList $ return continuations)



onlineRMSMC :: MonadSample m =>
  Int
  -> (forall x . Population m x -> Population m x)
  -> MSF (DunaiReader.ReaderT r2 (TrStat.Traced (Population m))) a2 b
  -> MSF (DunaiReader.ReaderT r2 m) a2 [(b, Log Double)]
onlineRMSMC nParticles resampler = withReaderS $ onlineRMSMC' nParticles resampler



onlineRMSMC' :: forall m a b . (Monad m, MonadSample m) =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x . Population m x -> Population m x)
  -> MSF (TrStat.Traced (Population m)) a b
  -> MSF m a [(b, Log Double)]
onlineRMSMC' nParticles resampler msf = onlineRMSMC'' $ lift (spawn nParticles) $> msf
  where
    onlineRMSMC'' :: TrStat.Traced (Population m) (MSF (TrStat.Traced (Population m)) a b) -> MSF m a [(b, Log Double)]
    onlineRMSMC'' msfs = MSF $ \a -> TrStat.marginal
      undefined -- return (currentPopulation, onlineRMSMC'' $ lift $ fromWeightedList $ return continuations)


withEndo :: (Endo a -> Endo b) -> (a -> a) -> b -> b
withEndo f = appEndo . f . Endo


composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k = withEndo (mconcat . replicate k)

-- runPopulationS :: forall m a b . Monad m =>
--   -- | Number of particles
--   Int ->
--   -- | Resampler
--   (forall x . Population m x -> Population m x)
--   -> MSF (Population m) a b
--   -> MSF m a [(b, Log Double)]
-- runPopulationS nParticles resampler msf = particleFilter' $ spawn nParticles $> msf
--   where
--     particleFilter' :: Population m (MSF (Population m) a b) -> MSF m a [(b, Log Double)]
--     particleFilter' msfs = MSF $ \a -> do
--       -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
--       bAndMSFs <- runPopulation $ flip unMSF a =<< msfs
--       -- FIXME This abominal lambda could be done away by using Weighted?
--       let (currentPopulation, continuations) = unzip $ (\((b, msf), weight) -> ((b, weight), (msf, weight))) <$> bAndMSFs
--       -- FIXME This normalizes, which introduces bias, whatever that means
--       return (currentPopulation, particleFilter' $ normalize $ resampler $ fromWeightedList $ return continuations)

observe :: (Monad m, MonadCond m) => MSF m (Log Double) ()
observe = arrM factor

pattern V1 :: a -> V.Vector 1 a
pattern V1 x <- (V.head->x)
  where
    V1 x = V.singleton x

type V2 = V.Vector 2
pattern V2 :: a -> a -> V2 a
pattern V2 x y <- (V.toList->[x,y])
  where
    V2 x y = V.fromTuple (x, y)


instance (Num n, Eq n, Floating n) => VectorSpace (V.Vector 2 n) n where
    zeroVector = 0
    x ^+^ y = x + y
    a *^ x = fmap (*a) x -- a * x
    (V2 q1 q2) `dot` (V2 q'1 q'2) = q1*q'1 + q2 * q'2

xCoord, yCoord :: V.Vector 2 a -> a
xCoord (V2 x y) = x
yCoord (V2 x y) = y

type Signal m time a = forall b . BehaviourF m time b a
type StochasticSignal a = forall td m . (MonadSample m, Diff td ~ Double) => Signal m td a
type StochasticSignalTransform c d = forall td m . (MonadSample m, Diff td ~ Double) => BehaviourF m td c d
type StochasticSignalTransformUnnormalized c d = forall td m . (MonadInfer m, Diff td ~ Double) => BehaviourF m td c d

type NormalizedDistribution = MonadSample
type UnnormalizedDistribution = MonadInfer

type Unnormalized = MonadCond

type Stochastic = MonadSample
type UnnormalizedStochastic = MonadInfer
type ReadsStdIn = MonadIO
type InputOutput = MonadIO
type Deterministic = Monad

type Feedback = MonadFix

type SignalFunction a b c = Process a b c

type Process constraint a b = forall m cl . (constraint m, Time cl ~ Double) => MSF
  (ReaderT
     (TimeInfo cl)
     m)
  a
  b

type (&) :: ((Type -> Type) -> Constraint) -> ((Type -> Type) -> Constraint) -> ((Type -> Type) -> Constraint)
type (&) c1 c2 m = (c1 m, c2 m)

type NoInput = ()


time :: Process Deterministic b Double
time = sinceStart



savediv :: (Eq p, Fractional p) => p -> p -> p
savediv x 0 = 0
savediv x y = x / y