{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}



{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}



{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE LiberalTypeSynonyms #-}


{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


module BetaBern where
import Inference (particleFilter, SignalFunction, Stochastic, type (&), Unnormalized, SMCSettings (n), params)
import FRP.Rhine.Gloss
import Example (Result (Result, particles, measured, latent), drawBall', drawParticle')
import Control.Monad.Bayes.Population
import Numeric.Log (Log (ln))
import Control.Monad.Bayes.Class

import Witch (into)
import qualified Data.Text as T
import Linear (V2(..))
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Foldable (Foldable(fold))



std :: Double
std = 0.5




prior :: SignalFunction Stochastic () Position
prior = fmap sigmoid walk1D

  where

    walk1D = proc _ -> do
        dacceleration <- constM (normal 0 2 ) -< ()
        acceleration <- decayIntegral 1 -< dacceleration
        velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
        position <- decayIntegral 1 -< velocity
        returnA -< position

    sigmoid x = 1 / (1 + exp x)

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

type Observation = Bool
type Position = Double

observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc p -> do
    n <- arrM bernoulli -< p
    returnA -< n

posterior :: SignalFunction (Stochastic & Unnormalized) Observation Position
posterior = proc bool -> do
  latent <- prior -< ()
  predicted <- observationModel -< latent
  arrM condition -< predicted == bool
  returnA -< latent


--   observe -< Exp $ log $ if bool then latent else (1 - latent)
-- posteriorPredictive ::StochasticSignalTransformUnnormalized Observation (Position, Int)
-- posteriorPredictive = proc obs -> do
--   predicted <- posterior -< obs
--   e <- edgeBy ((>0.5) . norm) -< predicted
--   returnA -< (predicted, e)







----------
-- display
----------

gloss :: SignalFunction Stochastic T.Text Picture
gloss = proc _ -> do
            actualPosition <- prior -< ()
            measuredPosition <- observationModel -< actualPosition
            samples <- particleFilter params {n = 150} posterior -< measuredPosition
            visualisation -< Result {
                                particles = first (\y -> V2 0 ((y-0.5)*5)) <$> samples
                                , measured = if measuredPosition then uncurry V2 (-1,-2) else uncurry V2 (1,-2)
                                , latent = (\y -> uncurry V2 (0,(y-0.5)*5) ) actualPosition
                                }


-- data Result = Result
--   {
--     --   estimate :: Position
--     -- stdDev :: Double
--    measured :: Observation
--   , latent :: Position
--   , particles :: [(Position, Log Double)]
--   }
--   deriving Show

visualisation :: Monad m => MSF m Result Picture
visualisation = proc Result { particles, measured, latent} -> do

  let V2 mx my = measured

  parts <- fold <$> mapMSF drawParticle' -< particles
  let obs =
        -- scale 150 150 $
        translate (into @Float (mx*150)) (into @Float (my*150)) $
        text $ if mx < 0 then "True" else "False"

        --  -< (measured, 0.05, red)
  history <- accumulateWith (\x l -> take 200 $ x : l) [] -< latent
--   drawBall -< (latent, 0.02, withAlpha 0.5 green)
--   let l@(V2 x y) = latent

  pic <- drawBalls -< (zip history (repeat 1), green)
  returnA -< pic <> obs <> parts

drawBalls :: (Monad m) => MSF
  m
  ([(V2 Double, Log Double)], Color)
  Picture
drawBalls = proc (history, c) -> do
    returnA -< scale 150 150 $ pictures
        [translate (into @Float x-z/50) (into @Float y) $
        color (withAlpha (into @Float $ exp $ 0.2 * ln a) c) $

        circleSolid 0.02 | (((x,y), a), z) <- zip (first toTuple <$> history) [1..] ]

toTuple (V2 x y) = (x,y)

drawParticle :: Monad m => MSF m (V2 Double, Log Double) Picture
drawParticle = proc (position, probability) -> do
  drawBall' -< (position, 0.04, withAlpha (into @Float $ exp $ 0.2 * ln probability) violet)


