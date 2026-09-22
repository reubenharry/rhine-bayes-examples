{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import BayesLite
import Control.Monad.Trans.MSF.Reader (ReaderT)
import Data.Kind (Constraint, Type)
import Data.MonadicStreamFunction
import Data.VectorSpace
import RhineCompat

-- | 2D vector (avoids a linear dependency on WASM).
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
type Feedback = MonadFix

type SignalFunction constraint a b =
  forall m cl.
  (constraint m, Time cl ~ Double) =>
  MSF (ReaderT (TimeInfo cl) m) a b

type (&) :: ((Type -> Type) -> Constraint) -> ((Type -> Type) -> Constraint) -> ((Type -> Type) -> Constraint)
type (&) c1 c2 m = (c1 m, c2 m)

type (>-->) a b = SignalFunction Stochastic a b
type (>-/->) a b = SignalFunction (Stochastic & Unnormalized) a b

observe :: (Monad m, MonadFactor m) => MSF m Double ()
observe = arrM score

decayingIntegral ::
  (Floating (Diff (Time cl)), Monad m, VectorSpace c (Diff (Time cl))) =>
  Diff (Time cl) ->
  ClSF m cl c c
decayingIntegral timeConstant = average timeConstant >>> arr (timeConstant *^)
