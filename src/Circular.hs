{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}

module Circular where


import Inference (SignalFunction, Stochastic, Unnormalized, type (&), particleFilter, Deterministic)
import Example (edgeBy, toTable, visualizeTable, renderObjects, Result (Result))
import Data.Fixed (mod')
import Data.MonadicStreamFunction
import Linear (V2)
import Linear (V2(..))
import FRP.Rhine.Gloss
import Example (glossClock)
import Control.Monad.Bayes.Population
import Control.Monad.Morph (MonadTrans(..))
import Numeric.Log (Log (ln))
import Control.Monad.Bayes.Sampler
import Linear.V2 (angle)
import Linear (unangle)
import Witch
import Control.Monad.Bayes.Class
import Control.Lens 
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Pong
import Prelude hiding (until)
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Example as E




-- a :: Bool
a ls = M.fromList ls & contains 1 %~ not

data Angle = Angle Double deriving (Eq, Ord)

type Position = V2 Double
type Observation = V2 Double

mkAngle x = Angle $ x `mod'` (2 *pi) 

type Length = Double

prior :: SignalFunction Stochastic () (Angle, Length)
prior = proc _ -> do
  x <- walk1D -< ()
  len <- (+1) . (/20) . abs <$> walk1D -< ()
  returnA -< (mkAngle x, len)


walk1D :: SignalFunction Stochastic () Double
walk1D = proc _ -> do
    dacceleration <- constM (normal 0 8 ) -< ()
    acceleration <- decayIntegral 3 -< dacceleration
    velocity <- decayIntegral 3 -< acceleration -- Integral, dying off exponentially
    position <- decayIntegral 3 -< velocity
    -- time <- count -< ()
    returnA -< position --  + 0.25 * sin (time/2)

    where
    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)



observationModel :: SignalFunction Stochastic (Angle, Length) Position
observationModel = proc (p, len) -> do
    n <- uncurry V2 <$> (noise &&& noise) -< ()
    returnA -< fmap (*len) (angle' p) + n

    where
        noise = constM (normal 0 std)

std :: Double
std = 0.1 

posterior :: SignalFunction (Stochastic & Unnormalized) Observation (Angle, Length)
posterior = proc (V2 oX oY) -> do
  latent <- prior -< ()
  pred@(V2 trueX trueY) <- observationModel -< latent
--   observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

predictive :: SignalFunction Stochastic (Angle, Length) Int
predictive = edgeBy (> mkAngle pi) <<< arr fst


mainSF :: SignalFunction (Stochastic & Unnormalized) Observation ((Angle, Length), Int)
mainSF = proc obs -> do
  predicted <- posterior -< obs
  e <- predictive -< predicted
  returnA -< (predicted, e)



----------
-- display
----------

angle' (Angle a) = angle a

gloss = undefined

-- gloss :: SignalFunction Stochastic T.Text Picture
-- gloss = proc _ -> do
--             actualPosition@(ang, len) <- prior -< ()
--             measuredPosition <- observationModel -< actualPosition
--             samples <- particleFilter 200 resampleMultinomial mainSF -< measuredPosition
--             guess <- arr toTable -< samples
--             (scene, table) <- renderObjects *** visualizeTable -< (E.Result {
--                                 -- E.particles = first ((\(a,l) -> fmap (*l) (angle' a)) . fst) <$> samples
--                                 -- , E.measured = measuredPosition
--                                 , E.latent =  fmap (*len) (angle' ang)
--                                 }, guess)
--             returnA -< scene

-- data Result = Result
--   {
--     --   estimate :: Position
--     -- stdDev :: Double
--    measured :: Observation
--   , latent :: Position
--   , particles :: [((Position), Log Double)]
--   }
--   deriving Show


-- visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
-- visualisation = proc Result { particles, latent, measured} -> do

--   let (V2 x y) = latent
--   drawBall -< (latent, 0.1, yellow)
--   drawBall -< (measured, 0.05, red)
--   arrMCl paintIO -< line [(0,0),  (into @Float x * 150, into @Float y * 150)]
--   drawParticles -< particles


-- drawBall :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color) ()
-- drawBall = proc (V2 x y, width, theColor) -> do
--     arrMCl paintIO -<
--         scale 150 150 $
--         translate (into @Float x) (into @Float y) $
--         color theColor $
--         circleSolid $
--         into @Float width

-- drawParticle :: MonadIO m => BehaviourF (GlossConcT m) td (V2 Double, Log Double) ()
-- drawParticle = proc (position, probability) -> do
--   drawBall -< (position, 0.05, withAlpha (into @Float $ exp $ 0.2 * ln probability) violet)

-- drawParticles :: MonadIO m => BehaviourF (GlossConcT m) td [(V2 Double, Log Double)] ()
-- drawParticles = proc particles -> do
--   case particles of
--     [] -> returnA -< ()
--     p : ps -> do
--       drawParticle -< p
--       drawParticles -< ps
