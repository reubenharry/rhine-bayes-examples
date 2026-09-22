{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler.Strict (SamplerIO, sampleIO)
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Fixed (mod')
import Data.Kind (Type)
import Data.Tuple (swap)
import FRP.Rhine
import FRP.Rhine.Gloss (GlossConcT)
import GHC.Base (Constraint)
import Linear (V2 (..))
import qualified Linear as L
import Witch

----
-- helpful names for common types, for user-facing readability
----

type Unnormalized = MonadFactor

type Stochastic = MonadDistribution

type UnnormalizedStochastic = MonadMeasure

type InputOutput = MonadIO

type Deterministic = Monad

type Feedback = MonadFix

-- | Gloss clocks in rhine-gloss 1.8 use 'Seconds Float' as their time domain.
type SignalFunction constraint a b =
  forall m cl.
  (constraint m, Time cl ~ Seconds Float) =>
  Automaton
    (ReaderT (TimeInfo cl) m)
    a
    b

type System m a b = SignalFunction m a b

type (&) :: ((Type -> Type) -> Constraint) -> ((Type -> Type) -> Constraint) -> ((Type -> Type) -> Constraint)
type (&) c1 c2 m = (c1 m, c2 m)

type (>-->) a b = SignalFunction Stochastic a b
type (>-/->) a b = SignalFunction (Stochastic & Unnormalized) a b
type (>-&->) a b = SignalFunction (Stochastic & Feedback) a b

-- a PopulationT of particles
type Particles a = [(a, Log Double)]

constantly :: (Monad m) => (a -> m b) -> Automaton m a b
constantly = arrM

-----
--- numerical helper code
-----

savediv :: (Eq p, Fractional p) => p -> p -> p
savediv _ 0 = 0
savediv x y = x / y

instance (From a b) => From (V2 a) (V2 b) where
  from :: (From a b) => V2 a -> V2 b
  from (V2 x y) = V2 (from x) (from y)

instance VectorSpace (V2 Double) Double where
  zeroVector = 0
  s *^ x = fmap (* s) x
  x ^+^ y = x + y
  x `dot` y = x `L.dot` y

newtype Angle = Angle Double deriving (Eq, Ord)

mkAngle :: Double -> Angle
mkAngle x = Angle $ x `mod'` (2 * pi)

angle' :: Angle -> V2 Double
angle' (Angle a) = L.angle a

averageOf :: (Functor t, Floating a, Foldable t) => t (a, Log a) -> a
averageOf things =
  let properThings = first (exp . ln) . swap <$> things
      fullWeight = Prelude.sum $ fst <$> properThings
      sumOfThings = Prelude.sum $ fmap (uncurry (*)) properThings
   in sumOfThings / fullWeight

stdDevOf :: (Floating b, Functor t, Foldable t, VectorSpace b b) => t (b, Log b) -> b
stdDevOf things =
  let av = averageOf things
      squares = first (\x -> norm (x - av) ** 2) <$> things
   in sqrt $ averageOf squares

expected :: (Floating a) => [(V2 a, Log a)] -> V2 a
expected v =
  V2
    (averageOf (fmap (\(V2 x _, w) -> (x, w)) v))
    (averageOf (fmap (\(V2 _ y, w) -> (y, w)) v))

normalPdf2D :: V2 Double -> Double -> V2 Double -> Log Double
normalPdf2D (V2 x1 y1) std (V2 x2 y2) = normalPdf x1 std x2 * normalPdf y1 std y2

safeNorm :: (Floating a, Num v, VectorSpace v a, Eq v) => v -> a
safeNorm 0 = 0
safeNorm x = norm x

-- | Leaky integrator compatible with gloss 'Seconds Float' clocks.
--   Matches the old @timeConstant *^ average timeConstant@ behaviour for Doubles.
decayingIntegral ::
  (Monad m, Diff (Time cl) ~ Seconds Float) =>
  Float ->
  ClSF m cl Double Double
decayingIntegral timeConstant = feedback 0 $ proc (x, y) -> do
  Seconds dt <- timeInfoOf sinceLast -< ()
  let tau = realToFrac timeConstant :: Double
      dt' = realToFrac dt :: Double
      y' = y + dt' * (x - y / tau)
  returnA -< (y', y')

---
-- reactive helper code
---

hold :: (Monad m) => a -> Automaton m (Maybe a) a
hold = lastS

observe :: (Monad m, MonadFactor m) => Automaton m (Log Double) ()
observe = arrM factor

-----
-- probabilistic helper code
-----

instance MonadFix SamplerIO where
  mfix f = liftIO (mfix (sampleIO . f))

instance (MonadDistribution m) => MonadDistribution (GlossConcT m) where
  random = lift random
