{-# LANGUAGE GADTs #-}

module Inference where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Weighted (runWeightedT)
import qualified Control.Monad.Bayes.Weighted as W
import Data.Automaton.Trans.Reader (readerS, runReaderS)
import Data.Functor (($>))
import Data.Stream.Result (Result (..))
import FRP.Rhine hiding (Result, normalize, readerS, runReaderS)

data SMCSettings m where
  SMCSettings ::
    { n :: Int
    , resampler :: forall x. PopulationT m x -> PopulationT m x
    } ->
    SMCSettings m

params :: (MonadDistribution m) => SMCSettings m
params = SMCSettings {n = 100, resampler = resampleSystematic}

particleFilter ::
  forall m cl a b.
  (Monad m) =>
  SMCSettings m ->
  ClSF (PopulationT m) cl a b ->
  ClSF m cl a [(b, Log Double)]
particleFilter config =
  readerS . (fmap snd . particleFilterDiscreteTime config) . runReaderS

particleFilterWithEvidence ::
  forall m cl a b.
  (Monad m) =>
  SMCSettings m ->
  ClSF (PopulationT m) cl a b ->
  ClSF m cl a (Log Double, [(b, Log Double)])
particleFilterWithEvidence config =
  readerS . particleFilterDiscreteTime config . runReaderS

particleFilterDiscreteTime ::
  forall m a b.
  (Monad m) =>
  SMCSettings m ->
  Automaton (PopulationT m) a b ->
  Automaton m a (Log Double, [(b, Log Double)])
particleFilterDiscreteTime config msf =
  go $ spawn (n config) $> msf
  where
    go ::
      PopulationT m (Automaton (PopulationT m) a b) ->
      Automaton m a (Log Double, [(b, Log Double)])
    go pop = unfoldM pop \a automata -> do
      (bAndAutomata, ld) <-
        runWeightedT $
          runPopulationT $
            extractEvidence $
              resampler config $ do
                auto <- automata
                Result nextAuto b <- stepAutomaton auto a
                pure (b, nextAuto)
      let (currentPopulation, continuations) =
            unzip $
              (\((b, sf), weight) -> ((b, weight), (sf, weight)))
                <$> bAndAutomata
      pure $
        Result
          (fromWeightedList $ pure continuations)
          (ld, currentPopulation)

-- | Normalizes the weights in the PopulationT so that their sum is 1.
normalize :: (Monad m) => PopulationT m a -> PopulationT m a
normalize = hoist W.unweighted . extractEvidence
