{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}


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
      GlossSettings(display), GlossConcT, red, withAlpha, green, paintIO, scale, text, translate )
import Control.Monad.Bayes.Sampler ( sampleIO )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import qualified Data.Vector.Sized as V
import Numeric.Hamilton ()
import Numeric.LinearAlgebra.Static ()
import Inference (pattern V2, onlineSMC)
import Example hiding (visualisation, Result, latent, measured, particles, std, posterior, generativeModel, prior)
import Data.Fixed (mod')
import Data.MonadicStreamFunction.InternalCore
import Numeric.Log
import qualified Data.Map as M
import Data.List (sortOn)
import Data.Ord (Down(..))


std :: Double
std = 0.5

-- bar :: Monad m => MSF m (x, Double) (x, Double)
-- bar = MSF \(x,t) -> do

--     undefined

switch :: (MonadIO m, Real (Diff cl), MonadSample m) => BehaviourF m (cl) () (Double, Bool)
switch = feedback True $ proc (_, d :: Bool) -> do
    -- t <- sinceInitS -< ()
    n <- count -< ()
    a <- constM (bernoulli 0.5) -< ()
    returnA -< if n `mod` 50 == 0 then (if a then (-1, True) else (1, False), a) else (if d then (-1, True) else (1, False), d) -- if a then (-1) else 1 else if not a then (-1) else 1

-- foo :: IO ()
-- foo = sampleIO $ reactimateCl (waitClock @100) (switch >>> arrM (liftIO . print))

prior :: (MonadSample m, Diff td ~ Double, MonadIO m) => BehaviourF m td () (Position, (Bool, Bool))
prior = fmap (first V.fromTuple . (\((d1,b1), (d2,b2)) -> ((d1,d2), (b1,b2)))) (model1D &&& model1D) where

    model1D = proc () -> do
        (n, b) <- switch -< ()
        acceleration <- constM (normal 0 4) -< ()
        velocity <- decayIntegral 2 -< acceleration -- Integral, dying off exponentially
        position <- decayIntegral 2 -< velocity + n
        returnA -< (max (-3) $ min 3 position, b)

    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

generativeModel :: (MonadSample m, Diff td ~ Double) => BehaviourF m td (Position, b) Observation
generativeModel = proc (p, b) -> do
    n <- fmap V.fromTuple $ noise &&& noise -< ()
    -- isOutlier <- constM (bernoulli 0.1) -< ()
    returnA -< p + n
    -- if isOutlier then fmap V.fromTuple $ outlier &&& outlier-< () else returnA -< p + n 

    where
        noise = constM (normal 0 std)
        -- outlier = constM ((\x -> 10 * (x - 0.5)) <$> random)


posterior :: (MonadInfer m, Diff td ~ Double, MonadIO m) => BehaviourF m td Observation (Position, (Bool, Bool))
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY, b) <- prior -< () -- fmap V.fromTuple $ (constM ((\x -> 10 * (x - 0.5)) <$> random)) &&& (constM ((\x -> 10 * (x - 0.5)) <$> random)) -< ()
--   observation <- generativeModel -< latent
  arrM factor -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< (latent)



----------
-- display
----------

gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ reactimateCl glossClock proc () -> do
            actualPosition <- prior -< ()
            measuredPosition <- generativeModel -< actualPosition
            samples <- onlineSMC 200 resampleMultinomial posterior -< measuredPosition
            let bs = head $ sortOn (Down . snd) $ M.toList $ M.mapKeys disp $ foldr (\(bb, kd) -> M.alter (\case Nothing -> Just kd; Just x ->  Just (x +kd) ) bb ) (mempty :: M.Map (Bool, Bool) (Log Double)) $  fmap (first snd) samples
            (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                particles = first fst <$> samples
                                , measured = measuredPosition
                                , latent = actualPosition
                                , direction = show bs
                                }


disp (True, True) = "Left Down"
disp (False, False) = "Right Up"
disp (True, False) = "Left Up"
disp (False, True) = "Right Down"

visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
visualisation = proc Result { particles, measured, latent, direction} -> do

  drawParticles -< particles
  drawBall -< (measured, 0.05, red)
  let (pos, bs) =  latent
  drawBall -< (pos, 0.3, withAlpha 0.5 green)
  arrMCl paintIO -< translate (-280) (220) $ scale 0.2 0.2 $ text ("Inferred " <> direction)
  arrMCl paintIO -< translate (-280) 250 $ scale 0.2 0.2 $ text ("True: " <> disp bs)

data Result = Result
  {
    --   estimate :: Position
    -- stdDev :: Double
   measured :: Observation
  , latent :: (Position, (Bool, Bool))
  , particles :: [(Position, Log Double)]
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
-- prior = fmap V.fromTuple $ model1D &&& model1D where

--     model1D = proc _ -> do
--         acceleration <- constM (normal 0 5) -< ()
--         velocity <- decayIntegral 2-< double2Float acceleration -- Integral, dying off exponentially
--         position <- decayIntegral 2-< velocity
--         returnA -< float2Double position

--     decayIntegral timeConstant = average timeConstant >>> arr (timeConstant *^)






