{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}


module ComplexExample where

import qualified Data.Vector.Sized as V
import FRP.Rhine hiding (runReaderS, readerS)
import Inference (pattern V2, V2, particleFilter, StochasticSignal, NormalizedDistribution, StochasticSignalTransform, StochasticSignalTransformUnnormalized, observe)
import FRP.Rhine.Gloss hiding (runReaderS, readerS)
import Numeric.Log
import GHC.Float
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Population
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Class
import Example ( Position, drawParticles, drawBall)
import Data.Fixed (mod')
import qualified Data.Map as M
import Control.Monad.Trans.List
import Control.Monad.Trans.MSF.List
import Control.Monad.Trans.MSF
import qualified Control.Category as C

std :: Double
std = 0.5

type Angle = Double

type Observation = (Angle, V.Vector 2 Double)
type SystemState = M.Map Int (V.Vector 2 Double)

bearing :: RealFloat a => V2 a -> a
bearing (V2 x y) = atan2 (y+2) (x+2)



prior :: (MonadSample m, Diff td ~ Double) => BehaviourF m td () (V.Vector 2 Double)
prior = fmap V.fromTuple $ walk1D &&& walk1D


walk1D :: (MonadSample m, Diff td ~ Double) => BehaviourF m td () Double
walk1D = proc _ -> do
    dacceleration <- constM (normal 0 12 ) -< ()
    acceleration <- decayIntegral 1.5 -< dacceleration
    velocity <- decayIntegral 1.5 -< acceleration -- Integral, dying off exponentially
    position <- decayIntegral 1.5 -< velocity
    -- returnA -< ((position `mod'` 4) - 2)
    returnA -< (max (-2) $ min 2 position)

    where decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

observationModel :: (MonadSample m, Diff td ~ Double) => BehaviourF m td (V.Vector 2 Double) (Double, V.Vector 2 Double)
observationModel = proc position@(V2 p1 p2) -> do
    angle <- arr bearing -< position
    noisyAngle <- arrM (`normal` 0.001) -< angle
    noisyPosition <- arrM (`normal` std) *** arrM (`normal` std) -< (p1,p2)
    returnA -< (noisyAngle, V.fromTuple noisyPosition)

posterior :: (MonadInfer m, Diff td ~ Double) => BehaviourF m td Observation (V.Vector 2 Double)
-- posterior ::StochasticSignalTransformUnnormalized Observation Position
posterior = proc (angle, V2 v1 v2) -> do
  latent <- prior -< ()
  (predictedAngle, V2 predV1 predV2) <- observationModel -< latent
  observe -< normalPdf predictedAngle std angle
  observe -< normalPdf predV1 std v1 * normalPdf predV2 std v2
  returnA -< latent

-- observationModel :: StochasticSignalTransform SystemState Observation
-- observationModel = proc ls -> do
-- --     n <- noise -< ()
--     y <- arrM uniformD -< M.keys ls
--     Just z <- arr (uncurry M.lookup) -< (y, ls)
--     returnA -< z

--     where 
--         noise = constM (normal 0 std)


-- posterior ::StochasticSignalTransformUnnormalized Observation Position
-- posterior = proc (V2 oX oY) -> do
--   latent@(V2 trueX trueY) <- prior -< ()
--   observe -< normalPdf oY std trueY * normalPdf oX std trueX
--   returnA -< latent


-- foo :: Bool
-- foo = mempty :: Bool

-- >>> foo







----------
-- display
----------

-- gloss = undefined


-- (e, _) <- initClock $ GlossEventClockIO
--         runIdentityT $ reactimateCl glossClock proc () -> do

--                 actualPosition <- prior -< ()
--                 measuredPosition <- observationModel -< actualPosition
--                 samples <- particleFilter 50 resampleMultinomial posterior -< measuredPosition
                -- (_, event) <- (readerS (constM (pure ()) >>> liftTransS e)) -< ()


glossClock :: RescaledClock GlossEventClockIO Double
glossClock = RescaledClock
  { unscaledClock = GlossEventClockIO
  , rescale = float2Double
  }


gloss :: IO ()
gloss = sampleIO $ 
            launchGlossThread defaultSettings
                { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
            do 
            -- (e, _) <- initClock GlossEventClockIO
            reactimateCl glossClock proc () -> do

                -- (_, event) <- (readerS (constM (pure ()) >>> liftTransS e)) -< ()

                n <- sinceLastS -< ()
                arrM (liftIO . print) -< n 

                actualPosition <- prior -< ()
                measuredPosition <- observationModel -< actualPosition
                samples <- particleFilter 200 resampleMultinomial posterior -< measuredPosition
                -- returnA -< undefined

                (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                    particles = samples
                                    , measured = measuredPosition
                                    , latent = actualPosition
                                    }




visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
visualisation = proc Result { particles, measured, latent} -> do

  let (angle, velocity) = measured
  drawParticles -< particles
  drawBall -< (V.fromTuple (cos angle, sin angle) - 2, 0.05, red)
  drawBall -< (velocity, 0.05, red)
  drawBall -< (latent, 0.3, withAlpha 0.5 green)

-- drawBall :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color) ()
-- drawBall = proc (V2 x y, width, theColor) -> do
--     arrMCl paintIO -< 
--         scale 150 150 $
--         translate (double2Float x) (double2Float y) $
--         pictures [
--         color theColor $
--         circleSolid $
--         double2Float width, scale 0.001 0.001 $ text "1"]




data Result = Result
  {
    --   estimate :: Position
    -- stdDev :: Double
   measured :: Observation
  , latent :: V.Vector 2 Double
  , particles :: [(V.Vector 2 Double, Log Double)]
  }
  deriving Show



-- experiment :: MonadSample m => MSF ( m) () SystemState
-- -- experiment :: (MonadSample m, Diff td ~ Double) => BehaviourF (ListT m) td () SystemState
-- -- experiment :: (Diff (Time cl) ~ Double, MonadSample m) => MSF
-- --   (ReaderT (TimeInfo cl) m) t SystemState
-- -- experiment :: MSF (ListT m÷) t1 t2

-- experiment = feedback undefined proc (_, prevState) -> do
--     newState <- arrM (\x -> do
--         b <- bernoulli 0.1
--         return if b then take 1 x else x) -< prevState

--     returnA -< undefined
    


-- prior :: (MonadSample m, Diff td ~ Double) => BehaviourF m td () SystemState
-- prior = proc _ -> do
--     p1 <- fmap V.fromTuple $ walk1D &&& walk1D -< ()
--     p2 <- fmap V.fromTuple $ walk1D &&& walk1D -< ()
--     -- y <- arr (: []) -< x
--     -- n <- count -< ()
--     -- z <- arrM undefined -< ()
--     returnA -< M.fromList [(0, p1), (1, p2)]
-- -- prior = fmap V.fromTuple $ walk1D &&& walk1D where

-- prior :: (MonadSample m, Diff td ~ Double) => BehaviourF m td () (V.Vector 2 Double)
-- prior = feedback (0,0) proc ((), fb@(fb1, fb2)) -> do
--     which <- bernoulliProcess -< ()
--     p1 <- if which then walk1D -< () else C.id -< fb1
--     p2 <- if not which then walk1D -< () else C.id -< fb2
--     returnA -< (V.fromTuple (p1, p2), (p1,p2)) 

-- bernoulliProcess :: MonadSample m => MSF (ReaderT (TimeInfo cl) m) () Bool
-- bernoulliProcess = feedback True proc ((), prevB) -> do
--     b1 <- constM (bernoulli 0.5 ) -< ()
--     b2 <- constM (bernoulli 0.5 ) -< ()
--     n <- count -< ()
--     returnA -< (if n `mod` 30 == 0 then b1 else prevB, if n `mod` 30 == 0 then b2 else prevB)
