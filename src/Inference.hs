{-# LANGUAGE GADTs #-}

module Inference where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import qualified Control.Monad.Bayes.Weighted as W
import qualified Control.Monad.Trans.MSF as D
import Data.Functor (($>))
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF (..))
import FRP.Rhine hiding (normalize)
import Numeric.Log
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.Bayes.Weighted (runWeightedT)

data SMCSettings m where
  SMCSettings :: {n :: Int, resampler :: forall x. PopulationT m x -> PopulationT m x} -> SMCSettings m

params :: MonadDistribution m => SMCSettings m
params = SMCSettings {n = 100, resampler = resampleSystematic}



particleFilter ::
  forall m cl a b.
  Monad m =>
  SMCSettings m ->
  ClSF (PopulationT m) cl a b ->
  ClSF m cl a [(b, Log Double)]
particleFilter config = D.readerS . (fmap snd . particleFilterDiscreteTime config) . D.runReaderS

particleFilterWithEvidence ::
  forall m cl a b.
  Monad m =>
  SMCSettings m ->
  ClSF (PopulationT m) cl a b ->
  ClSF m cl a (Log Double, [(b, Log Double)])
particleFilterWithEvidence config = D.readerS . particleFilterDiscreteTime config . D.runReaderS

particleFilterDiscreteTime ::
  forall m a b.
  Monad m =>
  SMCSettings m ->
  MSF (PopulationT m) a b ->
  MSF m a (Log Double, [(b, Log Double)])
particleFilterDiscreteTime config msf = particleFilter''' $ spawn (n config) $> msf
  where
    -- particleFilter'' :: PopulationT m (MSF (PopulationT m) a b) -> MSF m a [(b, Log Double)]
    -- particleFilter'' msfs = MSF \a -> do
    --   bAndMSFs <- runPopulationT $ normalize $ resampler config $ flip unMSF a =<< msfs
    --   let (currentPopulationT, continuations) =
    --         unzip $ (\((b, sf), weight) -> ((b, weight), (sf, weight))) <$> bAndMSFs
    --   return (currentPopulationT, particleFilter'' $ fromWeightedList $ return continuations)


    particleFilter''' :: PopulationT m (MSF (PopulationT m) a b) -> MSF m a (Log Double, [(b, Log Double)])
    particleFilter''' msfs = MSF \a -> do
      (bAndMSFs, ld) <- runWeightedT $ runPopulationT $ extractEvidence $ resampler config $ flip unMSF a =<< msfs
      let (currentPopulationT, continuations) =
            unzip $ (\((b, sf), weight) -> ((b, weight), (sf, weight))) <$> bAndMSFs
      return ((ld, currentPopulationT), particleFilter''' $ fromWeightedList $ pure continuations)

-- | Normalizes the weights in the PopulationT so that their sum is 1.
normalize :: (Monad m) => PopulationT m a -> PopulationT m a
normalize = hoist W.unweighted . extractEvidence