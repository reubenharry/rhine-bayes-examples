module Switch where

import Control.Lens (Field1 (_1), Field2 (_2), (%~), (&))
import Control.Monad.Bayes.Class
  ( MonadSample (bernoulli, normal),
    normalPdf,
  )
import Control.Monad.Trans.MSF.List (mapMSF)
import Data.Foldable (Foldable (fold))
import Data.List (sortOn)
import qualified Data.Map as M
import Data.MonadicStreamFunction
  ( Arrow (arr, first, (&&&)),
    constM,
    count,
    returnA,
    (>>>),
  )
import Data.MonadicStreamFunction.InternalCore (MSF, feedback)
import Data.Ord (Down (..))
import Data.Text (Text)
import Example (Observation, Position, drawBall')
import FRP.Rhine
  ( VectorSpace ((*^)),
    average,
  )
import FRP.Rhine.Gloss
  ( Color,
    Picture,
    green,
    red,
    scale,
    text,
    translate,
    withAlpha,
    yellow,
  )
import FRP.Rhine.Gloss.Common (blue)
import Inference
import Linear (V2 (..))
import Numeric.Log (Log (ln))
import Util
import Witch (into)

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
  returnA -< (uncurry V2 (x, y), (dir1, dir2))
  where
    walk1DSwitch = proc () -> do
      (n, b) <- switch -< ()
      acceleration <- constM (normal 0 4) -< ()
      velocity <- decayIntegral 1 -< acceleration -- Integral, dying off exponentially
      position <- decayIntegral 1 -< velocity + n
      returnA -< (max (-3) $ min 3 position, b)

    decayIntegral timeConstant = average timeConstant >>> arr (timeConstant *^)

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
  let bs = head $ sortOn (Down . snd) $ M.toList $ M.mapKeys disp $ foldr (\(bb, kd) -> M.alter (\case Nothing -> Just kd; Just x -> Just (x + kd)) bb) (mempty :: M.Map (Bool, Bool) (Log Double)) $ fmap (first snd) samples
  visualisation
    -<
      Result
        { particles = samples,
          measured = measuredPosition,
          latent = actualPosition,
          direction = show bs
        }

disp :: (Bool, Bool) -> String
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
visualisation = proc Result {particles, measured, latent, direction} -> do
  parts <- fold <$> mapMSF drawParticle -< particles & traverse . _1 . _2 %~ col
  obs <- drawBall' -< (measured, 0.05, red)
  let (pos, trueColor) = latent
  ball <- drawBall' -< (pos, 0.3, withAlpha 0.5 $ col trueColor)
  let infText = translate (-280) 220 $ scale 0.2 0.2 $ text ("Inferred " <> direction)
  let trueText = translate (-280) 250 $ scale 0.2 0.2 $ text ("True: " <> disp trueColor)
  returnA -< (parts <> obs <> ball <> infText <> trueText)

drawParticle :: Monad m => MSF m ((Position, Color), Log Double) Picture
drawParticle = proc ((position, c), probability) -> do
  drawBall' -< (position, 0.1, withAlpha (into @Float $ exp $ 0.2 * ln probability) c)

data Result = Result
  { measured :: Observation,
    latent :: (Position, (Bool, Bool)),
    particles :: [((Position, (Bool, Bool)), Log Double)],
    direction :: String
  }
  deriving (Show)
