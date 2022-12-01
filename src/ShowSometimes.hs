{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TupleSections #-}

module ShowSometimes where
-- import Control.Monad.Bayes.Population
--     ( resampleMultinomial )
-- import Data.MonadicStreamFunction
--     ( returnA, (>>>), arrM, constM, Arrow(first, arr, (&&&)), withSideEffect_ )
-- import Control.Monad.Bayes.Class
--     ( MonadSample(normal), factor, normalPdf )
-- import FRP.Rhine
--     ( VectorSpace((*^)),
--       arrMCl,
--       constMCl,
--       average,
--       liftClock,
--       reactimateCl,
--       LiftClock, integral )
-- import GHC.Float (float2Double, double2Float)
-- import FRP.Rhine.Gloss
--     ( blue,
--       green,
--       red,
--       withAlpha,
--       circleSolid,
--       color,
--       scale,
--       translate,
--       Display(InWindow),
--       defaultSettings,
--       clearIO,
--       launchGlossThread,
--       paintIO,
--       GlossSettings(display),
--       GlossConcT,
--       GlossSimClockIO(..), Event (EventKey), Key (Char), KeyState (Down) )
-- import Control.Monad.Bayes.Sampler ( sampleIO, SamplerIO )
-- import Control.Monad.Trans.Class ( MonadTrans(lift) )
-- import qualified Data.Vector.Sized as V
-- import Numeric.Hamilton ()
-- import Numeric.LinearAlgebra.Static ()
-- import Control.Monad.Trans.Identity ( IdentityT(runIdentityT) )
-- import Inference (pattern V2, xCoord, yCoord, NormalizedDistribution, StochasticSignal, StochasticSignalTransform, UnnormalizedDistribution, averageOf, stdDevOf, particleFilter)
-- import Example (observationModel)

-- std :: Double
-- std = 3

-- type Observation = V.Vector 2 Double
-- type Position = V.Vector 2 Double


-- prior :: NormalizedDistribution m => StochasticSignal m Position
-- prior = fmap V.fromTuple $ walk1D &&& walk1D where

--     walk1D = proc _ -> do
--         acceleration <- constM (normal 0 5) -< ()
--         velocity <- decayIntegral 2-< double2Float acceleration -- Integral, dying off exponentially
--         position <- decayIntegral 2-< velocity
--         returnA -< float2Double position

--     decayIntegral timeConstant = average timeConstant >>> arr (timeConstant *^)


-- posterior :: UnnormalizedDistribution m => StochasticSignalTransform m Observation Position
-- posterior = proc (V2 oX oY) -> do
--   latent@(V2 trueX trueY) <- prior -< ()
--   observe -< normalPdf oY std trueY * normalPdf oX std trueX
--   returnA -< latent



-- ----------
-- -- display
-- ----------

-- gloss :: IO ()
-- gloss = sampleIO $
--         launchGlossThread defaultSettings
--             { display = InWindow "rhine-bayes" (1024, 960) (10, 10) } 
--         do 
--         -- (e, _) <- initClock $ GlossEventClockIO
--         runIdentityT $ reactimateCl glossClock proc () -> do

--                 actualPosition <- prior -< ()
--                 measuredPosition <- observationModel -< actualPosition
--                 samples <- particleFilter 50 resampleMultinomial posterior -< measuredPosition
--                 -- (_, event) <- (readerS (constM (pure ()) >>> liftTransS e)) -< ()
--                 -- (readerS (constM (pure ()) >>> e) >>> arrM (lift . paintIO . text . show))
--                 (withSideEffect_ (lift $ lift clearIO) >>> visualisation) -< Result { estimate = averageOf samples
--                                     , stdDev = stdDevOf (first xCoord <$> samples) + stdDevOf (first yCoord <$> samples)
--                                     , measured = measuredPosition
--                                     , latent = actualPosition
--                                     , showEstimate = True -- isChar 'b' event
--                                     , showObservation = True -- isChar 'c' event
--                                     }


-- isChar c' (EventKey (Char c) Down _ _ ) | c'==c = True
-- isChar _ _ = False

-- data Result = Result
--   { estimate :: Position
--   , stdDev :: Double
--   , measured :: Observation
--   , latent :: Position
--   , showEstimate :: Bool
--   , showObservation :: Bool
--   }
--   deriving Show

-- glossClock :: LiftClock (GlossConcT SamplerIO) IdentityT GlossSimClockIO
-- glossClock = liftClock GlossSimClockIO


    
-- visualisation :: StochasticSignalTransform (IdentityT (GlossConcT SamplerIO)) Result ()
-- visualisation = proc Result { estimate, stdDev, measured, latent, showEstimate, showObservation } -> do
--   if showEstimate 
--       then drawBall -< (estimate, stdDev, blue)
--       else constM (pure ()) -< ()

--   if showObservation
--       then drawBall -< (measured, 0.3, red)
--       else constM (pure ()) -< ()

--   drawBall -< (latent, 0.3, withAlpha 0.5 green)

--   where
--     drawBall = proc (V2 x y, width, theColor) -> do
--         arrMCl $ lift . paintIO -<
--             scale 50 50 $
--             translate (double2Float x) (double2Float y) $
--             color theColor $
--             circleSolid $
--             double2Float width










