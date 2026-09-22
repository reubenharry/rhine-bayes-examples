{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Inference where

import BayesLite
import Control.Monad.Trans.MSF.Reader (readerS, runReaderS)
import Data.Functor (($>))
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF (..))
import RhineCompat (ClSF)

data SMCSettings m = SMCSettings
  { n :: Int
  , resampler :: forall x. PopulationT m x -> PopulationT m x
  }

params :: (MonadDistribution m) => SMCSettings m
params = SMCSettings {n = 40, resampler = resampleSystematic}

particleFilter ::
  forall m cl a b.
  (Monad m) =>
  SMCSettings m ->
  ClSF (PopulationT m) cl a b ->
  ClSF m cl a [(b, Double)]
particleFilter config =
  readerS . (fmap snd . particleFilterDiscreteTime config) . runReaderS

particleFilterDiscreteTime ::
  forall m a b.
  (Monad m) =>
  SMCSettings m ->
  MSF (PopulationT m) a b ->
  MSF m a (Double, [(b, Double)])
particleFilterDiscreteTime config msf = go $ spawn (n config) $> msf
  where
    go :: PopulationT m (MSF (PopulationT m) a b) -> MSF m a (Double, [(b, Double)])
    go msfs = MSF \a -> do
      bAndMSFs <- runPopulation $ resampler config $ flip unMSF a =<< msfs
      let (current, continuations) =
            unzip $
              (\((b, sf), weight) -> ((b, weight), (sf, weight)))
                <$> bAndMSFs
          -- evidence proxy: sum of weights after resampling is uninformative;
          -- keep a placeholder for API symmetry with the full library.
          ld = 0
      pure ((ld, current), go $ fromWeightedList $ pure continuations)
