{-# OPTIONS_GHC -Wno-missing-signatures #-}

module MainSF where

import Concurrent (UserInput, handle, noInput)
import Control.Concurrent (forkIO, newMVar, swapMVar)
import Control.Monad (forever, void)
import Control.Monad.Bayes.Sampler.Strict (sampleIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import qualified Data.Text.IO as T
import FRP.Rhine.Gloss hiding (Down, Up, forever, loop)
import Util

-- | Run a stochastic signal function in a Gloss window.
toGloss ::
  SignalFunction Stochastic (UserInput, Text) Picture ->
  IO ()
toGloss sf = do
  mvar <- newMVar ""
  void $
    forkIO $
      forever do
        x <- T.getLine
        swapMVar mvar x

  flowGlossIO defaultSettings {display = InWindow "rhine-bayes" (1200, 1000) (10, 10)} $
    tagS
      @@ GlossEventClockIO
        >-- collect
        --> hoistS
          (hoist (lift . sampleIO))
          ( proc events -> do
              inputText <- constM (liftIO (swapMVar mvar "")) -< ()
              gl <- accumulateWith handle noInput -< events
              out <- sf -< (gl, inputText)
              returnA -< out
          )
      >-> arrMCl paintAllIO
      @@ GlossSimClockIO
