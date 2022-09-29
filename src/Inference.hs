{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

module Inference where
import Control.Monad.Bayes.Population
import Data.MonadicStreamFunction
import Numeric.Log
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import Data.Functor (($>))
import Control.Monad.Bayes.Class
import FRP.Rhine hiding (normalize)
import GHC.Float (float2Double, double2Float)
import FRP.Rhine.Gloss hiding (normalize)
import Control.Monad.Bayes.Sampler
import qualified Control.Monad.Trans.MSF as DunaiReader
import Data.Tuple
import Control.Monad.Trans.Class
import qualified Data.Vector.Sized as V
import Numeric.Hamilton
import Numeric.LinearAlgebra.Static
import Control.Monad.Trans.Identity
import qualified Data.Vector as VVV
import Control.Monad.Trans.List
import Control.Applicative (Applicative(..))
import Control.Monad.Bayes.Traced.Dynamic (Traced)
import qualified Control.Monad.Bayes.Traced.Dynamic as Tr
import Data.Monoid (Endo (appEndo, Endo))
import qualified Control.Monad.Bayes.Traced.Basic as TrBas
import qualified Control.Monad.Bayes.Traced.Static as TrStat


-- todos

-- complex monadsample streams, e.g. beta or poisson etc for next position:
  -- or stay near circle / bounce off it
-- bearing example
-- user input
-- two streams: one is a boolean and when true, means dot is in top half
-- language


instance MonadSample m => MonadSample (GlossConcT m) where
  random = lift random

instance MonadSample m => MonadSample (ExceptT e m) where
  random = lift random

glossClock :: LiftClock (GlossConcT SamplerIO) IdentityT GlossSimClockIO
glossClock = liftClock GlossSimClockIO



onlineSMC :: forall m cl a b . Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x . Population m x -> Population m x)
  -> ClSF (Population m) cl a b
  -> ClSF m cl a [(b, Log Double)]
onlineSMC nParticles resampler msf = (withReaderS $ onlineSMC' nParticles resampler) msf

onlineRMSMC :: MonadSample m =>
  Int
  -> (forall x . Population m x -> Population m x)
  -> MSF (DunaiReader.ReaderT r2 (TrStat.Traced (Population m))) a2 b
  -> MSF (DunaiReader.ReaderT r2 m) a2 [(b, Log Double)]
onlineRMSMC nParticles resampler msf = (withReaderS $ onlineRMSMC' nParticles resampler) msf

withReaderS :: (Monad m1, Monad m2) =>
  (MSF m2 (r1, a1) b1 -> MSF m1 (r2, a2) b2)
  -> MSF (DunaiReader.ReaderT r1 m2) a1 b1
  -> MSF (DunaiReader.ReaderT r2 m1) a2 b2
withReaderS f = DunaiReader.readerS . f . DunaiReader.runReaderS

onlineSMC' :: forall m a b . Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x . Population m x -> Population m x)
  -> MSF (Population m) a b
  -> MSF m a [(b, Log Double)]
onlineSMC' nParticles resampler msf = onlineSMC'' $ spawn nParticles $> msf
  where
    onlineSMC'' :: Population m (MSF (Population m) a b) -> MSF m a [(b, Log Double)]
    onlineSMC'' msfs = MSF $ \a -> do
      -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
      bAndMSFs <- runPopulation $ normalize $ resampler $ flip unMSF a =<< msfs
      -- FIXME This abominal lambda could be done away by using Weighted?
      let (currentPopulation, continuations) = unzip $ (\((b, msf), weight) -> ((b, weight), (msf, weight))) <$> bAndMSFs
      -- FIXME This normalizes, which introduces bias, whatever that means
      return (currentPopulation, onlineSMC'' $ fromWeightedList $ return continuations)

onlineRMSMC' :: forall m a b . (Monad m, MonadSample m) =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x . Population m x -> Population m x)
  -> MSF (TrStat.Traced (Population m)) a b
  -> MSF m a [(b, Log Double)]
onlineRMSMC' nParticles resampler msf = onlineRMSMC'' $ (lift $ spawn nParticles) $> msf
  where
    onlineRMSMC'' :: TrStat.Traced (Population m) (MSF (TrStat.Traced (Population m)) a b) -> MSF m a [(b, Log Double)]
    onlineRMSMC'' msfs = MSF $ \a -> (TrStat.marginal) do
      -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
      -- bAndMSFs <- undefined -- composeCopies 1000 TrStat.mhStep $ flip unMSF a =<< msfs
      -- FIXME This abominal lambda could be done away by using Weighted?
      -- let (currentPopulation, continuations) = unzip $ (\((b, msf), weight) -> ((b, weight), (msf, weight))) <$> bAndMSFs
      -- FIXME This normalizes, which introduces bias, whatever that means
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
-- runPopulationS nParticles resampler msf = onlineSMC' $ spawn nParticles $> msf
--   where
--     onlineSMC' :: Population m (MSF (Population m) a b) -> MSF m a [(b, Log Double)]
--     onlineSMC' msfs = MSF $ \a -> do
--       -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
--       bAndMSFs <- runPopulation $ flip unMSF a =<< msfs
--       -- FIXME This abominal lambda could be done away by using Weighted?
--       let (currentPopulation, continuations) = unzip $ (\((b, msf), weight) -> ((b, weight), (msf, weight))) <$> bAndMSFs
--       -- FIXME This normalizes, which introduces bias, whatever that means
--       return (currentPopulation, onlineSMC' $ normalize $ resampler $ fromWeightedList $ return continuations)



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




