{-# LANGUAGE TupleSections #-}
module TwoStream where
import Control.Monad.Bayes.Class
    ( MonadSample(uniformD, normal), MonadInfer, normalPdf )
import FRP.Rhine
    ( arrMCl,
      reactimateCl,
      constM,
      Arrow((&&&), arr),
      MSF,
      returnA,
      (>>>),
      withSideEffect_,
      average,
      BehaviourF,
      TimeDomain(Diff),
      VectorSpace((*^)) )

import Example ( Position, Observation, std )
import GHC.Float ( double2Float, float2Double )
import Data.MonadicStreamFunction.InternalCore ( MSF(MSF) )
import FRP.Rhine.Gloss
    ( translate,
      scale,
      paintIO,
      defaultSettings,
      launchGlossThread,
      Display(InWindow),
      GlossSettings(display),
      blue,
      green,
      red,
      withAlpha,
      circleSolid,
      color,
      text,
      clearIO )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Bayes.Sampler ( sampleIO )
import Control.Monad.Trans.Identity ( IdentityT(runIdentityT) )
import Inference ( glossClock, particleFilter, observe, params )
import Control.Monad.Bayes.Population ( resampleMultinomial )
import Numeric.Log ( Log )
import Linear (V2(..))


prior :: (MonadSample m, Diff td ~ Float) => BehaviourF m td () (Position, Bool)
prior = proc () -> do
    b <- boolStream -< True
    m1 <- if b then walk1D -< () else constM (pure 0) -< ()
    m2 <- if not b then walk1D -< () else constM (pure 0) -< ()
    returnA -< (uncurry V2 (m1, m2), b) 
    
    where

    walk1D = proc _ -> do
        acceleration <- constM (normal 0 4 ) -< ()
        velocity <- decayIntegral 2 -< double2Float acceleration -- Integral, dying off exponentially
        position <- decayIntegral 2 -< velocity
        returnA -< (float2Double position)

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

-- observationModel :: NormalizedDistribution m => StochasticSignalTransform m (Position, Bool) Observation
observationModel = proc (p, _) -> do
    n <- fmap (uncurry V2) $ noise &&& noise -< ()
    returnA -< p + n

    where noise = constM (normal 0 std)


posterior :: (MonadInfer m, Diff td ~ Float) => BehaviourF m td Observation (Position, Bool)
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY, bool) <- prior -< ()
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent


boolStream :: MonadSample m => MSF m Bool Bool
boolStream = liftMSF (uniformD [constM (pure b) | b <- [True, False]])

liftMSF :: Monad m => m (MSF m a a) -> MSF m a a
liftMSF msf = MSF \x -> fmap (x,) msf


gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) } 
        $ runIdentityT $ reactimateCl glossClock proc () -> do

            -- bool <- boolStream -< True
            -- (withSideEffect_ (lift $ lift clearIO) >>> draw) -< bool
            latent@(actualPosition, b) <- prior -< ()
            -- returnA -< undefined
            measuredPosition <- observationModel -< latent
            samples <- particleFilter params (snd <$> posterior) -< measuredPosition
            (withSideEffect_ (lift $ lift clearIO) >>> visualisation) -< Result { estimate = 0 -- averageOf samples
                                , stdDev = 0 -- stdDevOf (first xCoord <$> samples) + stdDevOf (first yCoord <$> samples)
                                , measured = 0 -- measuredPosition
                                , latent = actualPosition
                                , isTrue = samples
                                }


-- visualisation :: StochasticSignalTransform (IdentityT (GlossConcT SamplerIO)) Result ()
visualisation = proc Result { estimate, stdDev, measured, latent, isTrue} -> do

  drawBall -< (estimate, stdDev, blue)
  drawBall -< (measured, 0.3, red)
  drawBall -< (latent, 0.3, withAlpha 0.5 green)
  arrMCl (lift . paintIO . scale 0.1 0.1 . text ) -< (show $ maxCommon isTrue)

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
  , isTrue :: [(Bool, Log Double)]
  }
  deriving Show


-- draw = proc bool -> do
--         arrMCl $ lift . paintIO -< text (show (bool :: Bool))