{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeApplications #-}
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


module Switch where
import Control.Monad.Bayes.Population
    ( resampleMultinomial )
import Data.MonadicStreamFunction
    ( returnA, (>>>), arrM, constM, Arrow(arr, (&&&), first, (***)), withSideEffect_, MSF, count )
import Control.Monad.Bayes.Class
    ( MonadSample(normal, bernoulli), factor, normalPdf, MonadInfer )
import FRP.Rhine
    ( VectorSpace((*^)),
      average,
      reactimateCl, TimeDomain (Diff), BehaviourF, absoluteS, waitClock, Millisecond, sinceInitS, TimeInfo (sinceLast), MonadIO (liftIO), arrMCl )
import FRP.Rhine.Gloss
    ( Display(InWindow),
      defaultSettings,
      clearIO,
      launchGlossThread,
      GlossSettings(display),
      GlossConcT,
      red,
      withAlpha,
      green,
      paintIO,
      scale,
      text,
      translate,
      yellow,
      addColors,
      Color )
import Control.Monad.Bayes.Sampler ( sampleIO )
import Control.Monad.Trans.Class ( MonadTrans(lift) )

import Numeric.Hamilton ()
import Numeric.LinearAlgebra.Static ()
import Inference (particleFilter, observe, SignalFunction, Stochastic, type (&), Unnormalized, params)
import Example hiding (Real, drawParticles, drawParticle, visualisation, Result, latent, measured, particles, std, posterior, observationModel, prior)
import Data.Fixed (mod')
import Data.MonadicStreamFunction.InternalCore
import Numeric.Log
import qualified Data.Map as M
import Data.List (sortOn)
import Data.Ord (Down(..))
import FRP.Rhine.Gloss.Common (blue)
import Witch
import Control.Lens
import Linear (V2(..))
import FRP.Rhine.Gloss (Picture)
import Data.Text (Text)
import Data.Foldable (fold)
import Control.Monad.Trans.MSF.List (mapMSF)


std :: Double
std = 0.5


switch :: SignalFunction Stochastic () (Double, Bool)
switch = feedback True $ proc (_, d :: Bool) -> do
    n <- count -< ()
    a <- constM (bernoulli 0.5) -< ()
    returnA -< if n `mod` 50 == 0 then (if a then (-1, True) else (1, False), a) else (if d then (-1, True) else (1, False), d) -- if a then (-1) else 1 else if not a then (-1) else 1


prior :: SignalFunction Stochastic () (Position, (Bool, Bool))
prior = proc () -> do

    (x, dir1) <- walk1DSwitch -< ()
    (y, dir2) <- walk1DSwitch -< ()
    returnA -< (uncurry V2 (x,y), (dir1, dir2))

    -- fmap (first uncurry V2 . (\((d1,b1), (d2,b2)) -> ((d1,d2), (b1,b2)))) (walk1D &&& walk1D) where

    where

    walk1DSwitch = proc () -> do
        (n, b) <- switch -< ()
        acceleration <- constM (normal 0 4) -< ()
        velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
        position <- decayIntegral 1 -< velocity + n
        returnA -< (max (-3) $ min 3 position, b)

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

observationModel :: SignalFunction Stochastic (Position, b) Observation
observationModel = proc (p, _) -> do
    n <- fmap (uncurry V2) $ noise &&& noise -< ()
    returnA -< p + n

    where
        noise = constM (normal 0 std)


posterior :: SignalFunction (Stochastic & Unnormalized) Observation (Position, (Bool, Bool))
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY, b) <- prior -< () -- fmap (uncurry V2) $ (constM ((\x -> 10 * (x - 0.5)) <$> random)) &&& (constM ((\x -> 10 * (x - 0.5)) <$> random)) -< ()
--   observation <- observationModel -< latent
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent



----------
-- display
----------

gloss :: SignalFunction Stochastic Text Picture
gloss = proc _ -> do
            actualPosition <- prior -< ()
            measuredPosition <- observationModel -< actualPosition
            samples <- particleFilter params posterior -< measuredPosition
            let bs = head $ sortOn (Down . snd) $ M.toList $ M.mapKeys disp $ foldr (\(bb, kd) -> M.alter (\case Nothing -> Just kd; Just x ->  Just (x +kd) ) bb ) (mempty :: M.Map (Bool, Bool) (Log Double)) $  fmap (first snd) samples
            visualisation -< Result {
                                particles = samples
                                , measured = measuredPosition
                                , latent = actualPosition
                                , direction = show bs
                                }


disp :: (Bool, Bool) -> [Char]
disp (True, True) = "Left Down"
disp (False, False) = "Right Up"
disp (True, False) = "Left Up"
disp (False, True) = "Right Down"

col :: (Bool, Bool) -> Color
col = \case
  (True, True) -> red
  (True, False) -> blue
  (False, True) -> green
  (False, False) -> yellow

visualisation :: Monad m => MSF m Result Picture
visualisation = proc Result { particles, measured, latent, direction} -> do

  parts <- fold <$> mapMSF drawParticle -< particles & traverse . _1 . _2 %~ col
  obs <- drawBall' -< (measured, 0.05, red)
  let (pos, trueColor) =  latent
  ball <- drawBall' -< (pos, 0.3, withAlpha 0.5 $ col trueColor)
  let infText = translate (-280) 220 $ scale 0.2 0.2 $ text ("Inferred " <> direction)
  let trueText = translate (-280) 250 $ scale 0.2 0.2 $ text ("True: " <> disp trueColor)
  returnA -< (parts <> obs <> ball <> infText <> trueText)


drawParticle :: Monad m => MSF m ((Position, Color), Log Double) Picture
drawParticle = proc ((position, c), probability) -> do
  drawBall' -< (position, 0.1, withAlpha (into @Float $ exp $ 0.2 * ln probability) c)

data Result = Result
  {
    --   estimate :: Position
    -- stdDev :: Double
   measured :: Observation
  , latent :: (Position, (Bool, Bool))
  , particles :: [((Position, (Bool, Bool)), Log Double)]
  , direction :: String -- (Bool, Bool)
  }
  deriving Show

-- -- c :: MonadSample m => Bool -> MSFExcept m (b, b') (Double, Bool) ()
-- c :: MonadSample m => b1 -> MSF (ExceptT () m) (a, b2) (Double, b1)
-- c b = proc (d,b) -> do 
--     state <- constM (normal 0 1) -< ()
--     bool <- constM (pure b) -< ()
--     (state', _) <- throwOnCond (\(x, _) -> x > 2) () -< (state, ())
--     returnA -< (state', bool)
--     -- (constM (normal 0 1) *** constM (pure b)) >>> throwOnCond (\(x, _) -> x > 2) ()

-- -- b :: MSFExcept IO a (Integer, Bool) ((), Bool)
-- -- b :: MSFExcept SamplerIO (b, Bool) (Double, Bool) ()
-- -- b :: MonadSample m => MSF (ExceptT () m) (Double, Bool) (Double, Bool)
-- -- b = proc x -> do 
-- --     (d,b) <- c True -< x
-- --     _ <- handleExceptT undefined -< ()
-- --     c False -< (d,b)
-- --     -- b -< y
--     -- b -< (d,b)


-- -- run :: IO (Either () ())
-- -- run = 
-- --     sampleIO $
-- --         launchGlossThread defaultSettings
-- --             { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
-- --         $ undefined 


-- -- run = sampleIO $ runExceptT $ reactimate $ constM (pure (1, True)) >>> b >>> arrM (\x -> lift (liftIO $ print x) >> return ()) 

-- -- counter :: MSF IO Bool Integer
-- -- counter = proc reset -> do
-- --         rec     
-- --                 -- num <- arr 
-- --                 n <- C.id -< n
-- --                 input <- count -< if n > 100 then 0 else 0 -- if input > 10 then 0 else next
-- --                 next <- (arrM (\x -> fmap (+x) (read <$> getLine))) -< input
-- --                 -- reset' <- arr not -< reset
-- --         returnA -< input

-- prior :: NormalizedDistribution m => StochasticSignal m Position
-- prior = fmap (uncurry V2) $ walk1D &&& walk1D where

--     walk1D = proc _ -> do
--         acceleration <- constM (normal 0 5) -< ()
--         velocity <- decayIntegral 2-< double2Float acceleration -- Integral, dying off exponentially
--         position <- decayIntegral 2-< velocity
--         returnA -< float2Double position

--     decayIntegral timeConstant = average timeConstant >>> arr (timeConstant *^)






