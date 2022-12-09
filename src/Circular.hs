
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

module Circular where


import Inference (SignalFunction, Stochastic, Unnormalized, type (&), particleFilter, observe, params)
import Example (edgeBy, toTable, visualizeTable, renderObjects)
import Data.Fixed (mod')
import Data.MonadicStreamFunction
import Linear ( V2, V2(..) )
import FRP.Rhine.Gloss
import Control.Monad.Bayes.Population
import Linear.V2 (angle)
import Witch
import Control.Monad.Bayes.Class
import Prelude hiding (until)
import qualified Data.Text as T
import qualified Example as E

std :: Double
std = 0.1

newtype Angle = Angle Double deriving (Eq, Ord)

type Position = V2 Double
type Observation = V2 Double

mkAngle :: Double -> Angle
mkAngle x = Angle $ x `mod'` (2 *pi)

type Length = Double

prior :: SignalFunction Stochastic () (Angle, Length)
prior = proc _ -> do
  x <- walk1D -< ()
  len <- (+1) . (/20) . abs <$> walk1D -< ()
  returnA -< (mkAngle x, len)




observationModel :: SignalFunction Stochastic (Angle, Length) Position
observationModel = proc (p, len) -> do
    n <- uncurry V2 <$> (noise &&& noise) -< ()
    returnA -< fmap (*len) (angle' p) + n
    where noise = constM (normal 0 std)

posterior :: SignalFunction (Stochastic & Unnormalized) Observation (Angle, Length)
posterior = proc (V2 oX oY) -> do
  latent <- prior -< ()
  V2 trueX trueY <- observationModel -< latent
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

predictive :: SignalFunction Stochastic (Angle, Length) Int
predictive = edgeBy (> mkAngle pi) <<< arr fst


mainSF :: SignalFunction (Stochastic & Unnormalized) Observation ((Angle, Length), Int)
mainSF = proc obs -> do
  predicted <- posterior -< obs
  e <- predictive -< predicted
  returnA -< (predicted, e)


walk1D :: SignalFunction Stochastic () Double
walk1D = proc _ -> do
    dacceleration <- constM (normal 0 8 ) -< ()
    acceleration <- decayIntegral 3 -< dacceleration
    velocity <- decayIntegral 3 -< acceleration -- Integral, dying off exponentially
    position <- decayIntegral 3 -< velocity
    returnA -< position --  + 0.25 * sin (time/2)

    where
    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)



----------
-- display
----------

angle' (Angle a) = angle a


gloss :: SignalFunction Stochastic T.Text Picture
gloss = proc _ -> do
            actualPosition@(ang, len) <- prior -< ()
            measuredPosition <- observationModel -< actualPosition
            samples <- particleFilter params mainSF -< measuredPosition
            guess <- arr toTable -< samples
            (scene, table) <- renderObjects *** visualizeTable -< (E.Result {
                                E.particles = first ((\(a,l) -> fmap (*l) (angle' a)) . fst) <$> samples
                                , E.measured = measuredPosition
                                , E.latent =  fmap (*len) (angle' ang)
                                }, guess)
            let V2 x y = fmap (*len) (angle' ang)
            let l = line [(0,0),  (into @Float x * 150, into @Float y * 150)]
            returnA -< scene <> table <> l
