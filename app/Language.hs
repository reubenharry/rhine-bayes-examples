{-# LANGUAGE GADTs #-}
module Main where

import Control.Category ((.))
import qualified DoublyDiscreteConvention
import Concurrent (UserInput, noInput)
import FRP.Rhine (ClSF, Clock (Time), reactimateCl, arrMCl, constM)
import FRP.Rhine.Gloss (Picture, morphS, GlossConcT, paintAllIO, launchGlossThread, defaultSettings, GlossSettings (display), Display (InWindow))
import Control.Monad.Bayes.Class (MonadDistribution)
import MainSF (eventClock)
import Prelude hiding ((.))
import Control.Monad.Bayes.Sampler.Strict (sampleIO)
import Control.Monad.Morph (MonadTrans(..), MFunctor (hoist))

-- react :: IO ()
react :: GlossConcT IO ()
react = reactimateCl eventClock (arrMCl paintAllIO . morphS (hoist (lift . sampleIO)) sf . constM (pure noInput))

sf :: (Time cl ~ Double, MonadDistribution m) => ClSF m cl
  UserInput
  Picture
sf = DoublyDiscreteConvention.main

main = launchGlossThread defaultSettings 
    {display = InWindow "rhine-bayes" (1724, 1260) (10, 10)}  react