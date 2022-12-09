{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}


module Bearing where


import FRP.Rhine hiding (runReaderS, readerS)
import Inference (particleFilter, StochasticSignal, NormalizedDistribution, StochasticSignalTransform, StochasticSignalTransformUnnormalized, observe, SignalFunction, Stochastic, type (&), Unnormalized, params)
import FRP.Rhine.Gloss hiding (runReaderS, readerS)
import Numeric.Log
import GHC.Float
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Population
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Class
import Example
    ( Position,
      drawParticles,
      drawBall,
      walk1D,
      Result(..),
      drawBall',
      glossClock, drawParticle' )
import Data.Fixed (mod')
import qualified Data.Map as M
import Control.Monad.Trans.List
import Control.Monad.Trans.MSF.List
import Control.Monad.Trans.MSF
import qualified Control.Category as C
import Linear (V2)
import Linear.V2 (V2(..))
import Circular (Angle (Angle), mkAngle, angle')
import Linear (angle)
import Linear (unangle)
import qualified Data.Text as T
import Witch (into)
import Data.Foldable (Foldable(fold))

std :: Double
std = 0.001


type Observation = Angle

bearing :: RealFloat a => V2 a -> a
bearing v = unangle (v + 2)


prior :: SignalFunction Stochastic () (V2 Double)
prior = fmap (uncurry V2) $ walk1D &&& walk1D

observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc position@(V2 p1 p2) -> do
    angle <- arr bearing -< position
    noisyAngle <- arrM (`normal` std) -< angle
    returnA -< mkAngle noisyAngle

posterior :: SignalFunction (Stochastic & Unnormalized) Observation (V2 Double)
posterior = proc (Angle angle) -> do
  latent <- prior -< ()
  Angle predictedAngle <- observationModel -< latent
  observe -< normalPdf predictedAngle std angle
  returnA -< latent


----------
-- display
----------



gloss :: SignalFunction Stochastic T.Text Picture
gloss = proc _ -> do

                actualPosition <- prior -< ()
                measuredPosition <- observationModel -< actualPosition
                samples <- particleFilter params posterior -< measuredPosition

                renderObjects -< Result {
                                    particles = samples
                                    , measured = angle' measuredPosition - 2
                                    , latent = actualPosition
                                    }




renderObjects ::  Monad m => MSF m Result Picture
renderObjects = proc Result { particles, measured, latent} -> do

  observation <- drawBall' -< (measured, 0.05, red)
  ball <- drawBall' -< (latent, 0.3, makeColorI 255 239 0 255)
  let V2 x y = measured
  let l = scale 150 150 $ line [(-2,-2), (into @Float x, into @Float y)]
  parts <- fold <$> mapMSF drawParticle' -< particles
  returnA -< (observation <> ball <> parts <> l)




