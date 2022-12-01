{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}



{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeOperators #-}

module Loop where
import FRP.Rhine.Gloss
import Example hiding (foo, posterior, observationModel, prior)
import Control.Monad.Bayes.Sampler
import Inference
import Control.Monad.Bayes.Population
import Control.Monad.Morph
import Control.Monad.Bayes.Class
import qualified Data.Vector.Sized as V
import Data.Void
import Control.Monad.Trans.MSF ()
import Control.Monad.Trans.Reader
import qualified Data.Text as T



type SumClock = Millisecond 1

foo :: (MonadSample m, Diff (Time cl) ~ Double, Time cl ~ Double) => ClSFExcept m cl () Position Empty
foo = do
    try $ abortivePrior
    foo

abortivePrior :: (MonadSample m, Diff (Time cl) ~ Double, Time cl ~ Double) => ClSF (ExceptT Position m) cl () Position
abortivePrior = proc () -> do
    x <- prior -< ()
    _ <- throwOn' -< (norm x > 2, x)
    returnA -< x


prior :: SignalFunction Stochastic () Position
prior = fmap V.fromTuple $ walk1D &&& walk1D where

    walk1D = proc _ -> do
        dacceleration <- constM (normal 0 8 ) -< ()
        acceleration <- integral -< dacceleration
        velocity <- integral -< acceleration -- Integral, dying off exponentially
        position <- integral -< velocity
        returnA -< position

observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc p -> do
    n <- fmap V.fromTuple $ noise &&& noise -< ()
    returnA -< p + n

    where 
        noise = constM (normal 0 0.2)


posterior ::SignalFunction (Stochastic & Unnormalized) Observation Position
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- man -< ()
  observe -< normalPdf oY 0.2 trueY * normalPdf oX 0.2 trueX
  returnA -< latent

man :: (MonadSample m, Diff (Time cl) ~ Double, Time cl ~ Double) => ClSF m cl () Position
man = safely foo 
-- >>> arrM (liftIO . print)
-- run = runExceptT $ reactimateCl (waitClock @100) sf

-- man2 :: IO ()
-- man2 = reactimateCl (waitClock @100) $ morphS (Control.Monad.Morph.hoist sampleIO) man

-- prior = undefined
    -- fmap V.fromTuple $ walk1D &&& walk1D where

    -- walk1D = proc _ -> do
    --     dacceleration <- constM (normal 0 8 ) -< ()
    --     acceleration <- decayIntegral 1 -< dacceleration
    --     velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
    --     position <- decayIntegral 1 -< velocity
    --     returnA -< position

    -- decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)


gloss :: SignalFunction Stochastic T.Text Picture
gloss = proc _ -> do
            actualPosition <- man -< ()
            measuredPosition <- observationModel -< actualPosition
            samples <- particleFilter 200 resampleMultinomial posterior -< measuredPosition
            renderObjects -< Result {
                                particles = samples
                                , measured = measuredPosition
                                , latent = actualPosition
                                }