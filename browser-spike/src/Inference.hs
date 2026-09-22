{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Inference where

import BayesLite
import Data.Automaton (Automaton, stepAutomaton, unfoldM)
import Data.Automaton.Trans.Reader (readerS, runReaderS)
import Data.Functor (($>))
import Data.Stream.Result (Result (..))
import RhineCompat (ClSF)

data SMCSettings m = SMCSettings
  { n :: Int
  , resampler :: forall x. PopulationT m x -> PopulationT m x
  }

params :: (MonadDistribution m) => SMCSettings m
params = SMCSettings {n = 50, resampler = resampleSystematic}

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
  Automaton (PopulationT m) a b ->
  Automaton m a (Double, [(b, Double)])
particleFilterDiscreteTime config msf =
  go $ spawn (n config) $> msf
  where
    go ::
      PopulationT m (Automaton (PopulationT m) a b) ->
      Automaton m a (Double, [(b, Double)])
    go pop = unfoldM pop \a automata -> do
      bAndAutomata <-
        runPopulation $
          resampler config $ do
            auto <- automata
            Result nextAuto b <- stepAutomaton auto a
            pure (b, nextAuto)
      let (current, continuations) =
            unzip $
              (\((b, sf), weight) -> ((b, weight), (sf, weight)))
                <$> bAndAutomata
      pure $
        Result
          (fromWeightedList $ pure continuations)
          (0, current)
