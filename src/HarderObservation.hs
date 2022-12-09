{-# OPTIONS_GHC -Wno-redundant-constraints #-}


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



module HarderObservation where
import Control.Monad.Bayes.Class
import FRP.Rhine
import Example (Position, Observation, Result (particles, Result, measured, latent), renderObjects, prior)

import FRP.Rhine.Gloss
import Inference (particleFilter, observe, SignalFunction, Stochastic, Unnormalized, type (&), params)
import Control.Monad.Bayes.Population (resampleMultinomial)
import Linear (V2)
import Linear.V2 (V2(..))
import Data.Text (Text)

type Acceleration = V2 Double




observationModel :: MonadSample m => BehaviourF m td Acceleration Observation
observationModel = proc a -> do
    n <- fmap (uncurry V2) $ noise &&& noise -< ()
    returnA -< a + n
    where
        noise = constM (normal 0 1)


posterior :: SignalFunction (Unnormalized & Stochastic) Observation Position
posterior = proc (V2 oAccX oAccY) -> do
  pos <- Example.prior -< ()
  V2 accX accY <- derivative >>> derivative -< pos
  observe -< normalPdf oAccY 1 accY * normalPdf oAccX 1 accX
  returnA -< pos

gloss :: SignalFunction Stochastic Text Picture
gloss = proc _ -> do
    actualPos <- prior -< ()
    actualAcc <- derivative >>> derivative -< actualPos
    measuredAcc <- observationModel -< actualAcc
    samples <- particleFilter params posterior -< measuredAcc
    renderObjects -< Result {
                        particles = samples
                        , measured = measuredAcc
                        , latent = actualPos
                        }
