{-# LANGUAGE TupleSections #-}

module TwoStreamContinuous where
import Control.Monad.Bayes.Class
import FRP.Rhine
import qualified Data.Vector.Sized as V
import Example hiding (posterior, observationModel, visualisation, Result, latent, measured, stdDev, estimate, glossClock, prior)
import GHC.Float
import Data.MonadicStreamFunction.InternalCore
import FRP.Rhine.Gloss
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Trans.Identity
import Inference
import Control.Monad.Bayes.Population
import Numeric.Log
import qualified Debug.Trace as D


prior :: (MonadSample m, Diff td ~ Float) => BehaviourF m td () (Position, Double)
prior = proc () -> do
    var <- doubleStream -< 1
    m1 <- walk1D -< var
    m2 <- walk1D -< var
    returnA -< (V.fromTuple (m1, m2), var) 
    
    where

    walk1D = proc var -> do
        acceleration <- arrM (normal 0 ) -< var
        velocity <- decayIntegral 2 -< double2Float acceleration -- Integral, dying off exponentially
        position <- decayIntegral 2 -< velocity
        returnA -< (float2Double position)

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

-- observationModel :: NormalizedDistribution m => StochasticSignalTransform m (Position, Double) Observation
observationModel = proc (p, _) -> do
    n <- fmap V.fromTuple $ noise &&& noise -< ()
    returnA -< p + n

    where noise = constM (normal 0 std)


posterior :: (MonadInfer m, Diff td ~ Float) => BehaviourF m td Observation (Position, Double)
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY, _) <- prior -< ()
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent


doubleStream :: MonadSample m => MSF m Double Double
doubleStream = liftMSF $ do
    x <- gamma 1 3
    return (constM $ pure x) 
    
    -- (uniformD [constM $ pure 1, constM $ pure 2] ) -- gamma 1 1])

liftMSF :: Monad m => m (MSF m a a) -> MSF m a a
liftMSF msf = MSF \x -> fmap (x,) msf


gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) } 
        $ runIdentityT $ reactimateCl glossClock proc () -> do

            -- bool <- boolStream -< True
            -- (withSideEffect_ (lift $ lift clearIO) >>> draw) -< bool
            latent@(actualPosition, trueD) <- prior -< ()
            -- returnA -< undefined
            measuredPosition <- observationModel -< latent
            samples <- particleFilter 100 resampleMultinomial (snd <$> posterior) -< measuredPosition
            (withSideEffect_ (lift $ lift clearIO) >>> visualisation) -< Result { estimate = 0 -- averageOf samples
                                , stdDev = 0 -- stdDevOf (first xCoord <$> samples) + stdDevOf (first yCoord <$> samples)
                                , measured = 0 -- measuredPosition
                                , latent = actualPosition
                                , doubleE = samples
                                , trueDouble = trueD
                                }


-- visualisation :: StochasticSignalTransform (IdentityT (GlossConcT SamplerIO)) Result ()
visualisation = proc Result { estimate, stdDev, measured, latent, doubleE, trueDouble} -> do

  drawBall -< (estimate, stdDev, blue)
  drawBall -< (measured, 0.3, red)
  drawBall -< (latent, 0.3, withAlpha 0.5 green)
  arrMCl (lift . paintIO . scale 0.1 0.1 . text ) -< (show $ doubleE)
  arrMCl (lift . paintIO . translate 0 (-10) . scale 0.1 0.1 . text ) -< (show $ trueDouble)

  where
    drawBall = proc (V2 x y, width, theColor) -> do
        arrMCl $ lift . paintIO -<
            scale 50 50 $
            translate (double2Float x) (double2Float y) $
            color theColor $
            circleSolid $
            double2Float width

    maxCommon ls = 
        let 
                fs = fst <$> ls
                s = filter id fs
                n = filter not fs
        in if s > n then "true" else "false"
    probTrue ls = foldr (\(b,p) s -> if b then s + p else p) 0 ls

data Result = Result
  { estimate :: Position
  , stdDev :: Double
  , measured :: Observation
  , latent :: Position
  , doubleE :: [(Double, Log Double)]
  , trueDouble :: Double
  }
  deriving Show


-- draw = proc bool -> do
--         arrMCl $ lift . paintIO -< text (show (bool :: Bool))