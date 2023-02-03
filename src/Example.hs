{-# OPTIONS_GHC -Wno-deprecations #-}
module Example where

import Concurrent (UserInput)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator (enumerate)
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Foldable (Foldable (fold))
import Data.List (intersperse)
import Data.Text (Text)
import Data.Tuple
import qualified Data.Vector as VV
import FRP.Rhine
import FRP.Rhine.Gloss
import GHC.Float
import Inference
import Linear (V2)
import qualified Linear as L
import Linear.V2 (V2 (..))
import Numeric.Log
import Witch (into)
import Prelude hiding (Real)
import Util

std :: Double
std = 1

type Real = Double

type Observation = V2 Double

type Position = V2 Double

prior :: SignalFunction Stochastic () Position
prior = proc _ -> do
  x <- walk1D -< ()
  y <- walk1D -< ()
  returnA -< V2 x y

observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc p -> do
  xNoise <- constM (normal 0 std) -< ()
  yNoise <- constM (normal 0 std) -< ()
  returnA -< p + V2 xNoise yNoise


posterior :: SignalFunction (Stochastic & Unnormalized) Observation Position
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- prior -< ()
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

gloss :: SignalFunction Stochastic Text Picture
gloss = proc _ -> do
  actualPosition <- prior -< ()
  measuredPosition <- observationModel -< actualPosition
  samples <- (particleFilter params posterior) -< measuredPosition
  renderObjects
    -<
      Result
        measuredPosition
        actualPosition
        samples

posteriorWeak :: SignalFunction (Stochastic & Unnormalized) Observation Position
posteriorWeak = proc (V2 oX oY) -> do
  (trueX, trueY) <- randomPosition &&& randomPosition -< ()
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< V2 trueX trueY
  where
    randomPosition = constM do
      uniform01 <- random
      return (10 * (uniform01 - 0.5))

weakPrior :: SignalFunction Stochastic Text Picture
weakPrior = proc _ -> do
  actualPosition <- prior -< ()
  measuredPosition <- observationModel -< actualPosition
  samples <- particleFilter params posteriorWeak -< measuredPosition
  renderObjects
    -<
      Result
        { particles = samples,
          measured = measuredPosition,
          latent = actualPosition
        }

walk1D :: SignalFunction Stochastic () Double
walk1D = proc _ -> do
  dacceleration <- constM (normal 0 8) -< ()
  acceleration <- decayingIntegral 1 -< dacceleration
  velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
  position <- decayingIntegral 1 -< velocity
  returnA -< position


decayingIntegral :: (Monad m, VectorSpace c (Diff (Time cl))) => 
  Diff (Time cl) -> ClSF m cl c c
decayingIntegral timeConstant = average timeConstant >>> arr (timeConstant *^)

-- | Harmonic oscillator with white noise
stochasticOscillator ::
  -- | Starting position
  Double ->
  -- | Starting velocity
  Double ->
  SignalFunction Stochastic Double Double
stochasticOscillator initialPosition initialVelocity = feedback 0 $ proc (stdDev, position') -> do
  impulse <- arrM (normal 0) -< stdDev
  let acceleration = (-3) * position' + impulse
  velocity <- arr (+ initialVelocity) <<< decayingIntegral 100 -< acceleration
  position <- integralFrom initialPosition -< velocity
  returnA -< (position, position)

countCrosses :: SignalFunction Deterministic Position Int
countCrosses = proc pos -> do
  isInsideRadius <- arr ((> 0.5) . L.norm) -< pos
  bool <- edge -< isInsideRadius
  returnA -< bool

postPred :: SignalFunction (Stochastic & Unnormalized) Observation (Position, Int)
postPred = proc obs -> do
  inferredPos <- posterior -< obs
  numTimesCrossRadius <- countCrosses -< inferredPos
  returnA -< (inferredPos, numTimesCrossRadius)

main :: SignalFunction Stochastic UserInput Picture
main = proc _ -> do
  actualPos <- prior -< ()
  measPos <- observationModel -< actualPos
  samples <- particleFilter params postPred -< measPos
  numberOfCrosses <- arr toTable -< samples
  objects <- renderObjects -< Result measPos actualPos (first fst <$> samples)
  probTable <- visualizeTable -< numberOfCrosses
  returnA -< objects <> probTable

----------
-- display
----------


toTable :: (Show b, Ord b) => [((a, b), Log Double)] -> [String]
toTable = intersperse "\n" . fmap (\(x, p) -> "Number: " <> show x <> "       Probability: " <> show p) . enumerate . empirical . fmap (first snd)

empirical :: MonadSample m => [(b, Log Double)] -> m b
empirical ls = do
  let (vs, ps) = unzip ls
  i <- logCategorical $ VV.fromList ps
  return (vs !! i)

edge :: Monad m => MSF m Bool Int
edge = feedback 0 proc (b, prev) -> do
  oldB <- iPre True -< b
  returnA -< (prev + if oldB == b then 0 else 1, prev + if oldB == b then 0 else 1)

edge' :: Monad m => MSF m Bool Bool
edge' = feedback False proc (b, prev) -> do
  returnA -< (prev == b, b)

edgeBy :: Monad m => (a -> Bool) -> MSF m a Int
edgeBy p = proc a -> do
  b <- arr p -< a
  e <- edge -< b
  returnA -< e

visualisation :: MonadIO m => Diff td ~ Double => BehaviourF (GlossConcT m) td Result ()
visualisation = proc Result {particles, measured, latent} -> do
  drawBall -< (measured, 0.05, red)
  drawBall -< (latent, 0.15, makeColorI 255 239 0 255)
  drawParticles -< particles

renderObjects :: Monad m => MSF m Result Picture
renderObjects = proc Result {particles, measured, latent} -> do
  observation <- drawBall' -< (measured, 0.05, red)
  ball <- drawBall' -< (latent, 0.15, makeColorI 255 239 0 255)
  parts <- fold <$> mapMSF drawParticle' -< particles
  returnA -< (observation <> ball <> parts)

visualizeTable :: Monad m => MSF m [String] Picture
visualizeTable = proc str -> do
  let table = translate 0 200 $ scale 0.1 0.1 $ pictures $ [translate 0 (height * 100) $ text l | (l, height) <- zip (reverse str) [1 ..]]
  let circ = circle 75
  returnA -< table <> circ

drawBall :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color) ()
drawBall = proc (V2 x y, width, theColor) -> do
  arrMCl paintIO
    -<
      scale 150 150 $
        translate (double2Float x) (double2Float y) $
          color theColor $
            circleSolid $
              double2Float width

drawParticle :: MonadIO m => BehaviourF (GlossConcT m) td (Position, Log Double) ()
drawParticle = proc (position, probability) -> do
  drawBall -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) violet)

drawParticles :: MonadIO m => BehaviourF (GlossConcT m) td [(Position, Log Double)] ()
drawParticles = proc particles -> do
  case particles of
    [] -> returnA -< ()
    p : ps -> do
      drawParticle -< p
      drawParticles -< ps

-- drawBall' :: MonadIO m => BehaviourF (GlossConcT m) cl (V2 Double, Double, Color) Picture
drawBall' :: Monad m => MSF m (V2 Double, Double, Color) Picture
drawBall' = proc (V2 x y, width, theColor) -> do
  returnA
    -<
      scale 150 150 $
        translate (double2Float x) (double2Float y) $
          color theColor $
            circleSolid $
              into @Float width

drawParticle' :: Monad m => MSF m (Position, Log Double) Picture
drawParticle' = proc (position, probability) -> do
  drawBall' -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) violet)

data Result = Result
  { --   estimate :: Position
    -- stdDev :: Double
    measured :: Observation,
    latent :: Position,
    particles :: [(Position, Log Double)]
  }
  deriving (Show)

moveAwayFrom :: SignalFunction Stochastic Position Position
moveAwayFrom = feedback 0 proc (otherBall, prevPos) -> do
  dacceleration <- constM (normal 0 8) &&& constM (normal 0 8) -< ()
  acceleration <- decayingIntegral 1 -< uncurry V2 dacceleration
  let repulsion =
        fmap
          (savediv (safeNorm (prevPos - otherBall)))
          (prevPos - otherBall)
  velocity <- decayingIntegral 1 -< acceleration + repulsion -- Integral, dying off exponentially
  position <- decayingIntegral 1 -< velocity

  returnA -< (position, position)

drawTriangle :: (V2 Double, Double, Float, Color) -> Picture
drawTriangle (V2 p1 p2, dir, size, col) =
  Color col $
    scale 50 50 $
      translate (into @Float p1) (into @Float p2) $
        rotate (- (360 / (2 * pi)) * into @Float dir) $
          scale size size $
            polygon [(0, 0.5), (0, -0.5), (2, 0), (0, 0.5)]
