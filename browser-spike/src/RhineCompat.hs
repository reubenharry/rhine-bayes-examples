{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Minimal Rhine-shaped clock / ClSF layer on top of dunai.
-- Full @rhine@ does not yet build against WASM GHC 9.14 (base bounds),
-- but existing demos only need TimeInfo + a few DSP helpers.
module RhineCompat
  ( TimeInfo (..)
  , DtClock (..)
  , ClSF
  , BehaviourF
  , BehaviorF
  , timeInfo
  , timeInfoOf
  , sinceLastS
  , sinceInitS
  , sinceStart
  , integral
  , integralFrom
  , average
  , averageFrom
  , mkTimeInfo
  , stepClSF
  , Diff
  , TimeDomain (..)
  , Time
  , Tag
  )
where

import Control.Arrow
import Control.Monad.Trans.MSF.Reader (ReaderT, ask, asks, runReaderS)
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF (..))
import Data.VectorSpace

type family Diff time

class TimeDomain time where
  diffTime :: time -> time -> Diff time
  addTime :: time -> Diff time -> time

type instance Diff Double = Double

instance TimeDomain Double where
  diffTime = (-)
  addTime = (+)

data TimeInfo cl = TimeInfo
  { sinceLast :: Diff (Time cl)
  , sinceInit :: Diff (Time cl)
  , absolute :: Time cl
  , tag :: Tag cl
  }

data DtClock = DtClock

type family Time cl
type family Tag cl

type instance Time DtClock = Double
type instance Tag DtClock = ()

type ClSF m cl a b = MSF (ReaderT (TimeInfo cl) m) a b
type BehaviourF m time a b = forall cl. (time ~ Time cl) => ClSF m cl a b
type BehaviorF m time a b = BehaviourF m time a b

timeInfo :: (Monad m) => ClSF m cl a (TimeInfo cl)
timeInfo = constM ask

timeInfoOf :: (Monad m) => (TimeInfo cl -> b) -> ClSF m cl a b
timeInfoOf f = constM (asks f)

sinceLastS :: (Monad m) => ClSF m cl a (Diff (Time cl))
sinceLastS = timeInfoOf sinceLast

sinceInitS :: (Monad m) => ClSF m cl a (Diff (Time cl))
sinceInitS = timeInfoOf sinceInit

sinceStart :: (Monad m, TimeDomain time, Num (Diff time)) => BehaviourF m time a (Diff time)
sinceStart = feedback Nothing $ proc (_, mStart) -> do
  t <- timeInfoOf absolute -< ()
  case mStart of
    Nothing -> returnA -< (0, Just t)
    Just t0 -> returnA -< (diffTime t t0, Just t0)

sumFrom :: (Monad m, VectorSpace v s) => v -> MSF (ReaderT r m) v v
sumFrom v0 = feedback v0 $ arr $ \(dv, v) ->
  let v' = v ^+^ dv
   in (v', v')

integralFrom ::
  (Monad m, VectorSpace v s, s ~ Diff td) =>
  v ->
  BehaviourF m td v v
integralFrom v0 = proc v -> do
  dt <- timeInfoOf sinceLast -< ()
  sumFrom v0 -< dt *^ v

integral ::
  (Monad m, VectorSpace v s, s ~ Diff td) =>
  BehaviourF m td v v
integral = integralFrom zeroVector

weightedAverageFrom ::
  (Monad m, VectorSpace v s, Num s) =>
  v ->
  BehaviourF m td (v, s) v
weightedAverageFrom v0 = feedback v0 $ proc ((v, weight), vAvg) -> do
  let vAvg' = weight *^ vAvg ^+^ (1 - weight) *^ v
  returnA -< (vAvg', vAvg')

averageFrom ::
  (Monad m, VectorSpace v s, Floating s, s ~ Diff td) =>
  v ->
  Diff td ->
  BehaviourF m td v v
averageFrom v0 t = proc v -> do
  TimeInfo {sinceLast = dt} <- timeInfo -< ()
  let weight = exp $ -(dt / t)
  weightedAverageFrom v0 -< (v, weight)

average ::
  (Monad m, VectorSpace v s, Floating s, s ~ Diff td) =>
  Diff td ->
  BehaviourF m td v v
average = averageFrom zeroVector

mkTimeInfo :: Double -> Double -> TimeInfo DtClock
mkTimeInfo sinceInit' sinceLast' =
  TimeInfo
    { sinceLast = sinceLast'
    , sinceInit = sinceInit'
    , absolute = sinceInit'
    , tag = ()
    }

-- | One simulation step of a clocked signal function.
stepClSF ::
  (Monad m) =>
  ClSF m cl a b ->
  TimeInfo cl ->
  a ->
  m (b, ClSF m cl a b)
stepClSF clsf ti a = unMSF (runReaderS clsf) (ti, a)
