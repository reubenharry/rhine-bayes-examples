{-# LANGUAGE TypeApplications #-}
module Lazy where
import Inference
import Data.MonadicStreamFunction (constM)
import Control.Arrow (returnA)
import Control.Arrow ((>>>))
import Data.MonadicStreamFunction.Core (arrM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import FRP.Rhine (reactimateCl)
import Control.Monad.Bayes.Sampler
import Data.MonadicStreamFunction (reactimate)
import Control.Monad.Trans.MSF (performOnFirstSample)
import Control.Monad.Bayes.Class

-- foo :: SignalFunction Stochastic () [Int]
-- foo = performOnFirstSample $ fmap undefined $ uniformD $ sequence $ repeat random

baz :: SamplerIO [Double]
baz = do x <- random 
         (x :) <$> baz

-- bar :: SignalFunction Stochastic Int Int
-- bar = proc i -> do
--     is <- foo -< ()
--     returnA -< is !! i
gloss = undefined
-- gloss = sampleIO $ reactimate (constM (read <$> liftIO getLine) >>> bar >>> arrM (liftIO . print))