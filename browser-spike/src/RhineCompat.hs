{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Minimal Rhine-shaped TimeInfo / ClSF on automaton (no rhine package).
module RhineCompat
  ( TimeInfo (..)
  , DtClock (..)
  , ClSF
  , timeInfo
  , timeInfoOf
  , sinceLast
  , sinceInit
  , absolute
  , tag
  , mkTimeInfo
  , Time
  , Tag
  , Diff
  )
where

import Control.Monad.Trans.Reader (ReaderT, ask, asks)
import Data.Automaton (Automaton, constM)

type family Diff time
type instance Diff Float = Float
type instance Diff Double = Double

data TimeInfo cl = TimeInfo
  { sinceLast :: Diff (Time cl)
  , sinceInit :: Diff (Time cl)
  , absolute :: Time cl
  , tag :: Tag cl
  }

data DtClock = DtClock

type family Time cl
type family Tag cl
type instance Time DtClock = Float
type instance Tag DtClock = ()

type ClSF m cl a b = Automaton (ReaderT (TimeInfo cl) m) a b

timeInfo :: (Monad m) => ClSF m cl a (TimeInfo cl)
timeInfo = constM ask

timeInfoOf :: (Monad m) => (TimeInfo cl -> b) -> ClSF m cl a b
timeInfoOf f = constM (asks f)

mkTimeInfo :: Float -> Float -> TimeInfo DtClock
mkTimeInfo sinceInit' sinceLast' =
  TimeInfo
    { sinceLast = sinceLast'
    , sinceInit = sinceInit'
    , absolute = sinceInit'
    , tag = ()
    }
