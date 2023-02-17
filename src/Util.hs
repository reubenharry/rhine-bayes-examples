{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util where
import Control.Monad.Bayes.Class
import Control.Monad.Fix (MonadFix (mfix))
import FRP.Rhine
import Control.Monad.Trans.MSF (ReaderT, performOnFirstSample)
import Data.Kind (Type)
import GHC.Base (Constraint)
import Witch
import Linear (V2, _x, _y)
import Linear.V2 (V2(..))
import qualified Linear as L
import Numeric.Log (Log (ln))
import Control.Monad.Bayes.Sampler.Strict (SamplerIO, sampleIO)
import FRP.Rhine.Gloss (GlossConcT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Tuple (swap)
import Data.Fixed (mod')
import Control.Lens (view)
import Data.Void (Void)
import Control.Monad (forever)
import Data.MonadicStreamFunction.InternalCore

----
-- helpful names for common types, for user-facing readability
----

type Unnormalized = MonadFactor

type Stochastic = MonadDistribution

type UnnormalizedStochastic = MonadMeasure

type InputOutput = MonadIO

type Deterministic = Monad

type Feedback = MonadFix

type SignalFunction constraint a b =
  forall m cl.
  (constraint m, Time cl ~ Double) =>
  MSF
    ( ReaderT
        (TimeInfo cl)
        m
    )
    a
    b

type (&) :: ((Type -> Type) -> Constraint) -> ((Type -> Type) -> Constraint) -> ((Type -> Type) -> Constraint)

type (&) c1 c2 m = (c1 m, c2 m)

-----
--- numerical helper code
-----

savediv :: (Eq p, Fractional p) => p -> p -> p
savediv _ 0 = 0
savediv x y = x / y

instance From a b => From (V2 a) (V2 b) where
  from :: From a b => V2 a -> V2 b
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

stdDevOf :: (Functor t, Foldable t, VectorSpace b b) => t (b, Log b) -> b
stdDevOf things =
  let av = averageOf things
      squares = first (\x -> norm (x - av) ** 2) <$> things
   in sqrt $ averageOf squares

expected :: Floating a => [(V2 a, Log a)] -> V2 a
expected v =
  V2
    (averageOf (fmap (first (view _x)) v))
    (averageOf (fmap (first (view _y)) v))

normalPdf2D :: V2 Double -> Double -> V2 Double -> Log Double
normalPdf2D (V2 x1 y1) std (V2 x2 y2) = normalPdf x1 std x2 * normalPdf y1 std y2

safeNorm :: (Num v, VectorSpace v a, Eq v) => v -> a
safeNorm 0 = 0
safeNorm x = norm x

toggle :: Bool -> SignalFunction Deterministic Bool Bool
toggle initialVal = safely $ forever do
  try proc bool -> do
    pos <- constM (pure initialVal) -< ()
    throwOn' -< (bool, pos)
    returnA -< pos
  try $ not initialVal <$ timer 0.01
  try proc bool -> do
    pos <- constM (pure (not initialVal)) -< ()
    throwOn' -< (bool, pos)
    returnA -< pos
  try $ initialVal <$ timer 0.01




bernoulliProcess :: (Time cl ~ Double, MonadDistribution m) => Bool -> Double -> ClSF m cl a Bool
bernoulliProcess b i = safely (switch b i) where 
  
  switch :: (Time cl ~ Double, MonadDistribution m) => Bool -> Double -> ClSFExcept m cl a Bool Void
  switch a d = do 
    x <- try proc _ -> do
      -- b <- performOnFirstSample $ (constM . pure) <$> (bernoulli 0.5) -< () 
      -- t <- sinceStart -< ()
      b <- constM $ bernoulli d -< ()
      throwOn' -< (b, not a)
      returnA -< a
    try $ x <$ timer 0.01
    switch x d

runNSteps (MSF msf) 1 input = 
    fst <$> msf input
runNSteps (MSF msf) n input = do
    (mid, nextMsf) <- msf input
    runNSteps nextMsf (n-1) input -- (n-1) mid
      

---
-- reactive helper code
---

hold :: Monad m => a -> MSF m (Maybe a) a
hold a = feedback a proc (x, old) -> do
  case x of
    Just y -> returnA -< (y, y)
    Nothing -> returnA -< (old, old)

time :: SignalFunction Deterministic b Double
time = sinceStart

observe :: (Monad m, MonadFactor m) => MSF m (Log Double) ()
observe = arrM factor



-----
-- probabilistic helper code
-----

instance MonadFix SamplerIO where
  mfix f = liftIO (mfix (sampleIO . f))


instance MonadDistribution m => MonadDistribution (GlossConcT m) where
  random = lift random

  
