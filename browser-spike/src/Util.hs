{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import BayesLite
import Control.Arrow (returnA)
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Automaton (Automaton, arrM, feedback)
import Data.Kind (Constraint, Type)
import Data.VectorSpace
import RhineCompat
import System.Random (StdGen)

data V2 a = V2 a a
  deriving (Eq, Show, Functor)

instance (Num a) => Num (V2 a) where
  V2 x1 y1 + V2 x2 y2 = V2 (x1 + x2) (y1 + y2)
  V2 x1 y1 - V2 x2 y2 = V2 (x1 - x2) (y1 - y2)
  V2 x1 y1 * V2 x2 y2 = V2 (x1 * x2) (y1 * y2)
  negate (V2 x y) = V2 (negate x) (negate y)
  abs (V2 x y) = V2 (abs x) (abs y)
  signum (V2 x y) = V2 (signum x) (signum y)
  fromInteger n = V2 (fromInteger n) (fromInteger n)

instance VectorSpace (V2 Double) Double where
  zeroVector = V2 0 0
  s *^ V2 x y = V2 (s * x) (s * y)
  V2 x1 y1 ^+^ V2 x2 y2 = V2 (x1 + x2) (y1 + y2)
  V2 x1 y1 `dot` V2 x2 y2 = x1 * x2 + y1 * y2

type Unnormalized = MonadFactor
type Stochastic = MonadDistribution
type Deterministic = Monad

type SignalFunction constraint a b =
  forall m cl.
  (constraint m, Time cl ~ Float, Diff (Time cl) ~ Float) =>
  Automaton (ReaderT (TimeInfo cl) m) a b

-- Class-based constraint conjunction (type synonyms cannot be unsaturated).
class (c1 m, c2 m) => (c1 & c2) m
instance (c1 m, c2 m) => (c1 & c2) m

type (>-->) a b = SignalFunction Stochastic a b
type (>-/->) a b = SignalFunction (Stochastic & Unnormalized) a b

observe :: (Monad m, MonadFactor m) => Automaton m Double ()
observe = arrM score

instance MonadFix SamplerIO where
  mfix f = SamplerIO $ StateT $ \g ->
    mfix $ \(a, _) ->
      let SamplerIO (StateT k) = f a
       in k g

-- | Matches live-branch decayingIntegral (Seconds Float ~ Float here).
decayingIntegral ::
  (Monad m, Diff (Time cl) ~ Float) =>
  Float ->
  ClSF m cl Double Double
decayingIntegral timeConstant = feedback 0 $ proc (x, y) -> do
  dt <- timeInfoOf sinceLast -< ()
  let tau = realToFrac timeConstant :: Double
      dt' = realToFrac dt :: Double
      y' = y + dt' * (x - y / tau)
  returnA -< (y', y')

normalPdf2D :: V2 Double -> Double -> V2 Double -> Double
normalPdf2D (V2 x1 y1) std (V2 x2 y2) =
  normalPdf x1 std x2 + normalPdf y1 std y2
