{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Minimal probabilistic programming surface for the WASM spike.
-- Intentionally tiny: enough for a particle filter, no Hackage monad-bayes.
module BayesLite where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT, get, put)
import Data.List (foldl')
import System.Random (StdGen, mkStdGen, randomR)

-- | Distributions we can sample from.
class (Monad m) => MonadDistribution m where
  random :: m Double -- Uniform[0,1)
  normal :: Double -> Double -> m Double

-- | Soft evidence / weighting.
class (Monad m) => MonadFactor m where
  score :: Double -> m () -- natural-log weight contribution
  factor :: Double -> m ()
  factor = score

type MonadMeasure m = (MonadDistribution m, MonadFactor m)

-- | Log-density of a univariate normal (natural log).
normalPdf :: Double -> Double -> Double -> Double
normalPdf mu sigma x =
  let v = sigma * sigma
   in (-0.5) * (log (2 * pi * v) + (x - mu) * (x - mu) / v)

-- | IO sampler with an explicit RNG in StateT (no mwc-random / vector).
newtype SamplerIO a = SamplerIO {unSamplerIO :: StateT StdGen IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

sampleIO :: SamplerIO a -> IO a
sampleIO (SamplerIO m) = evalStateT m (mkStdGen 42)

instance MonadDistribution SamplerIO where
  random = SamplerIO $ do
    g <- get
    let (u, g') = randomR (0.0, 1.0) g
    put g'
    pure u
  normal mu sigma = do
    -- Box–Muller
    u1 <- random
    u2 <- random
    let r = sqrt (-2 * log (max 1e-12 u1))
        theta = 2 * pi * u2
    pure (mu + sigma * r * cos theta)

-- | Population of weighted particles (log-weights).
newtype PopulationT m a = PopulationT {runPopulationT :: m [(a, Double)]}

instance (Functor m) => Functor (PopulationT m) where
  fmap f (PopulationT m) = PopulationT (fmap (map (\(a, w) -> (f a, w))) m)

instance (Monad m) => Applicative (PopulationT m) where
  pure a = PopulationT (pure [(a, 0)])
  (<*>) = apPop

apPop :: (Monad m) => PopulationT m (a -> b) -> PopulationT m a -> PopulationT m b
apPop (PopulationT mf) (PopulationT ma) = PopulationT $ do
  fs <- mf
  as <- ma
  pure [(f a, wf + wa) | (f, wf) <- fs, (a, wa) <- as]

instance (Monad m) => Monad (PopulationT m) where
  PopulationT m >>= k = PopulationT $ do
    as <- m
    concat <$> mapM (\(a, w) -> fmap (map (\(b, w') -> (b, w + w'))) (runPopulationT (k a))) as

instance MonadTrans PopulationT where
  lift m = PopulationT (fmap (\a -> [(a, 0)]) m)

instance (MonadDistribution m) => MonadDistribution (PopulationT m) where
  random = lift random
  normal mu s = lift (normal mu s)

instance (Monad m) => MonadFactor (PopulationT m) where
  score w = PopulationT (pure [((), w)])

instance (MonadDistribution m) => MonadDistribution (ReaderT r m) where
  random = lift random
  normal mu s = lift (normal mu s)

instance (MonadFactor m) => MonadFactor (ReaderT r m) where
  score = lift . score

spawn :: (Monad m) => Int -> PopulationT m ()
spawn n = PopulationT (pure (replicate n ((), 0)))

fromWeightedList :: (Monad m) => m [(a, Double)] -> PopulationT m a
fromWeightedList = PopulationT

runPopulation :: (Monad m) => PopulationT m a -> m [(a, Double)]
runPopulation = runPopulationT

-- | Systematic resampling; returns renormalized (log) weights of 0.
resampleSystematic :: (MonadDistribution m) => PopulationT m a -> PopulationT m a
resampleSystematic pop = PopulationT $ do
  particles <- runPopulationT pop
  case particles of
    [] -> pure []
    _ -> do
      let maxW = maximum (map snd particles)
          weights = map (\(_, w) -> exp (w - maxW)) particles
          total = sum weights
          probs = map (/ total) weights
          cum = scanl1 (+) probs
          n = length particles
      u0 <- random
      let starts = [(u0 + fromIntegral i) / fromIntegral n | i <- [0 .. n - 1]]
          pick u = fst (particles !! idx)
            where
              idx = length (takeWhile (< u) cum)
      pure [(pick u, 0) | u <- starts]

empirical :: (MonadDistribution m) => [(a, Double)] -> m a
empirical [] = error "empirical: empty"
empirical particles = do
  let maxW = maximum (map snd particles)
      weights = map (\(_, w) -> exp (w - maxW)) particles
      total = foldl' (+) 0 weights
      probs = map (/ total) weights
      cum = scanl1 (+) probs
  u <- random
  pure $ fst (particles !! length (takeWhile (< u) cum))
