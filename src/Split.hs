{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
module Split where


import Inference (StochasticSignal, V2(..), pattern V2, particleFilter, observe)
import Example (Position, glossClock, Observation)
import qualified Data.Vector.Sized as V
import Control.Arrow
import Data.MonadicStreamFunction
import Control.Monad.Bayes.Class
import FRP.Rhine (average, VectorSpace ((*^)))
import FRP.Rhine.Gloss
import Control.Monad.Morph (MonadTrans(..))
import Control.Monad.Bayes.Sampler
import Control.Monad.Trans.MSF.List (sequenceS, ListT, widthFirst)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Category as C
import Control.Monad.Fix
import qualified Control.Monad.Morph as MM
import Control.Concurrent
import Control.Monad
import Example qualified hiding (drawBall, drawParticles, visualisation)
import Control.Monad.Bayes.Population
import Numeric.Log
import Witch (into)

-- prior :: StochasticSignal Position
-- prior :: (MonadSample m) => Diff (Time cl) ~ Double => MSF
--   (ListT
--      (ReaderT (TimeInfo cl) m))
--   (Maybe String)
--   (V.Vector 2 Double)
prior :: MonadSample m => MSF
  (ListT
     (ReaderT
        (TimeInfo (RescaledClock GlossSimClockIO Double))
        (m)))
  (Maybe [Char])
  Position
prior = proc n -> do
  x <- morphS lift Example.prior -< ()
  y <- case n of 
    Just "split" -> split -< x
    _ -> C.id -< x
  returnA -< y

walk1D :: MonadSample m => Diff (Time cl) ~ Double => MSF
  (
     ReaderT (TimeInfo cl) m)
  t
  Double
walk1D = proc _ -> do
    dacceleration <- constM (normal 0 8 ) -< ()
    acceleration <- decayIntegral 1 -< dacceleration
    velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
    position <- decayIntegral 1 -< velocity
    -- time <- count -< ()
    returnA -< position --  + 0.25 * sin (time/2)

  where
    decayIntegral timeConstant =  average timeConstant >>> arr (timeConstant *^)

split :: Monad m => MSF (ListT m) b b
split = sequenceS [C.id, C.id]


-- instance MonadFix m => MonadFix (GlossConcT m) where
--     mfix f = lift (mfix f)


-- observationModel :: StochasticSignalTransform Position Observation
observationModel :: (MonadSample m) => MSF
  m
  [(V.Vector 2 Double)]
  (V.Vector 2 Double)
observationModel = proc p -> do
    -- n <- fmap V.fromTuple $ noise &&& noise -< ()
    q <- arrM uniformD -< p
    returnA -< q

noise :: MonadSample m => MSF m () Double
noise = constM (normal 0 std)


std = 0.1


-- posterior :: (MonadSample m) => Diff (Time cl) ~ Double => MSF
--   (ListT
--      (ReaderT (TimeInfo cl) m))
--   (Maybe String)
--   (V.Vector 2 Double)
-- posterior = undefined

-- posterior ::StochasticSignalTransformUnnormalized Observation Position
-- posterior :: (MonadInfer m, Diff (Time cl) ~ Double) => MSF
--   ( (ReaderT (TimeInfo cl) m)) (V.Vector 2 Double, Maybe String) [V.Vector 2 Double]
posterior :: MonadInfer m => MSF
  (ReaderT
     (TimeInfo (RescaledClock GlossSimClockIO Double))
     (m))
  (V2 Double, Maybe [Char])
  [Position]
posterior = proc (V2 oX oY, str) -> do
  latent <- widthFirst prior -< str
  pred@(V2 trueX trueY) <- observationModel -< latent
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

noisify = proc (V2 x y) -> do 
                        (n1,n2) <- noise *** noise -< ((),())
                        returnA -< V.fromTuple (x+n1, y+n2)

gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
        $ do
            mvar <- liftIO $ newMVar ""
            _ <- liftIO $ void $ forkIO $ forever do 
                x <- getLine 
                swapMVar mvar x
                print "foo"
                -- (liftIO $ void $ forkIO $ getLine >>= \x -> do print "foo"; void $ swapMVar mvar x) -< ()
            reactimateCl glossClock proc () -> do

                    message' :: String <- constM (liftIO $ swapMVar mvar "") -< ()
                    let message = if (not . null) message' then Just message' else Nothing
                    actualPosition <- 
                        -- morphS (MM.hoist (lift :: Monad m => m a -> GlossConcT m a)) 
                        (widthFirst prior) -< message
                    measuredPosition <- observationModel >>> noisify -< actualPosition
                    samples <- particleFilter 50 resampleMultinomial undefined -< (measuredPosition, message)
                    (withSideEffect_ (lift clearIO) >>> visualisation) -< Result {
                                        particles = samples
                                        , measured = measuredPosition
                                        , latent = actualPosition
                                        }

visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
visualisation = proc Result { particles, measured, latent} -> do

  drawParticles -< particles
  drawBall -< (measured, 0.05, red, "")
  drawBalls -< (\(x,y) -> (x, 0.3, withAlpha 0.5 green, y) )<$> (zip latent $ show <$> [1..])


drawParticles :: MonadIO m => BehaviourF (GlossConcT m) td [([Position], Log Double)] ()
drawParticles = proc particles -> do
  case particles of
    [] -> returnA -< ()
    p : ps -> do
      drawParticle -< p
      drawParticles -< ps

drawParticle :: MonadIO m => BehaviourF (GlossConcT m) td ([Position], Log Double) ()
drawParticle = proc (position, probability) -> do
  drawBalls -< (\(x,y) -> (x, 0.1, withAlpha (into @Float $ exp $ 0.2 * ln probability) violet, y)) <$> (zip position $ show <$> [1..])


-- drawBalls :: MonadIO m => BehaviourF (m) td [Position] ()
drawBalls :: MonadIO m => MSF
  (ReaderT (TimeInfo cl) (GlossConcT m))
  [(V2 Double, Double, Color, String)]
  ()
drawBalls = proc balls -> do
  case balls of
    [] -> returnA -< ()
    p : ps -> do
      drawBall -< p
      drawBalls -< ps

drawBall :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color, String) ()
drawBall = proc (V2 x y, width, theColor, str) -> do
    arrMCl paintIO -<
        scale 150 150 $
        translate (into @Float x) (into @Float y) $
        pictures [color theColor $
        circleSolid $
        into @Float width] -- , scale 0.001 0.001 $ text str]

data Result = Result
  {
    --   estimate :: Position
    -- stdDev :: Double
   measured :: Observation
  , latent :: [Position]
  , particles :: [([Position], Log Double)]
  }
  deriving Show