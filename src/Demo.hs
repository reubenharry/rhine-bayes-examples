module Demo where

import Concurrent (UserInput, keys, mouse)
import Control.Category as C
import Control.Lens
import Control.Monad.Bayes.Class hiding (prior, posterior)
import Control.Monad.Trans.MSF (MSFExcept)
import Control.Monad.Trans.MSF.Reader (ReaderT)
import Inference
import FRP.Rhine hiding (loop)
import FRP.Rhine.Gloss hiding (loop)
import Smoothing (prior)
import GHC.Float
import GUI
  ( ButtonConfig (buttonColor, buttonInitialVal, buttonPos),
    button,
    buttonParams,
    multipleSwitch,
    slider,
  )
import Linear (V2, unangle, _x, _y)
import Linear.Metric (Metric (..))
import Linear.V2 (V2 (..))
import Numeric.Log
import Witch (into)
import Prelude hiding (lines, Real, (.))
import Example (Result(..), prior, renderObjects, moveAwayFrom, drawTriangle, stochasticOscillator, walk1D, edgeBy)
import Util

type Real = Double

-- the type of observations
type Observation = Maybe (V2 Double)

type Position = V2 Double

type Std = Double

groundTruth :: SignalFunction Stochastic UserInput Position
groundTruth =
  multipleSwitch signals 1
  where
    signals 1 = oscillator
    signals 2 = \v -> proc _ -> (+ v) <$> Example.prior -< ()
    signals 3 = const weakPrior
    signals 4 = moveWithArrows
    signals 5 = moveInLines
    signals 6 = gravity
    signals _ = gravity


-- Using the slightly less user friendly version of the types here, for technical reasons
oscillator :: (MonadDistribution m, Time cl ~ Double) => V2 Double -> ClSF m cl t (V2 Double)
oscillator (V2 x y) = fmap (+ V2 x (y -2)) proc _ -> do
  xAxis <- Example.stochasticOscillator 0 2 -< 1
  yAxis <- Example.stochasticOscillator 2 0 -< 1
  returnA -< V2 xAxis yAxis

moveWithArrows :: (MonadDistribution m, Time cl ~ Double) => V2 Double -> ClSF m cl UserInput (V2 Double)
moveWithArrows pos = proc userInput -> do
  let velD = if userInput ^. keys . contains (Char 'd') then V2 1 0 else 0
  let velA = if userInput ^. keys . contains (Char 'a') then V2 (-1) 0 else 0
  let velW = if userInput ^. keys . contains (Char 'w') then V2 0 1 else 0
  let velS = if userInput ^. keys . contains (Char 's') then V2 0 (-1) else 0
  fmap (+ pos) integral -< velD + velA + velW + velS

moveInLines :: (MonadDistribution m, Time cl ~ Double) => V2 Double -> ClSF m cl t (V2 Double)
moveInLines v = fmap (+ v) (safely lines)
  where

    lines :: (Double ~ Time cl, Stochastic m) => MSFExcept (ReaderT (TimeInfo cl) m) a (V2 Double) b
    lines = loop 0
      where
        loop i = do
          xB <- once $ const $ bernoulli 0.5
          yB <- once $ const $ bernoulli 0.5
          point <- try $ straightLine i (V2 (if xB then 1 else (-1)) (if yB then 1 else (-1)))
          loop point

    straightLine :: (Double ~ Time cl, Monad m) => V2 Double -> V2 Double -> ClSF (ExceptT (V2 Double) m) cl a (V2 Double)
    straightLine p dir = proc _ -> do
      t <- sinceStart -< ()
      l <- arr (+ p) . integral  -< dir
      throwOn' -< (t > 1, l)
      returnA -< l

-- gravity :: V2 Double -> SignalFunction Stochastic a (V2 Double)
gravity :: (Monad m, Time cl ~ Double) => V2 Double -> ClSF m cl t (V2 Double)
gravity v =
  fmap
    ((_y %~ max 0) . (+ v))
    ( proc _ -> do
        vel <- integral -< V2 0 (-1)
        pos <- integral -< vel
        returnA -< pos
    )

weakPrior :: SignalFunction Stochastic UserInput Position
weakPrior = proc _ -> do
  (trueX, trueY) <- randomPosition &&& randomPosition -< ()
  returnA -< V2 trueX trueY
  where
    randomPosition = constM do
      uniform01 <- random
      return (10 * (uniform01 - 0.5))



generativeModel :: SignalFunction Stochastic (Position, Bool) Observation
generativeModel = proc (p, showObservation) -> do
  returnA -< if showObservation then Just p else Nothing

observationModel :: SignalFunction Stochastic (Position, Bool, Std) Observation
observationModel = proc (p, showObservation, std) -> do
  (x, y) <- (noise &&& noise) -< std
  returnA -< if showObservation then Just $ p + V2 x y else Nothing
  where
    noise = arrM (normal 0)

posterior :: SignalFunction (Stochastic & Unnormalized) (Observation, Bool, Std, UserInput) Position
posterior = proc (obs, showObs, std, stay) -> do
  latent <- groundTruth -< stay
  predictedObs <- generativeModel -< (latent, showObs)
  case (obs, predictedObs) of
    (Just (V2 oX oY), Just (V2 trueX trueY)) ->
      observe -< normalPdf oY std trueY * normalPdf oX std trueX
    (Nothing, Nothing) -> returnA -< ()
    _ -> arrM factor -< 0
  returnA -< latent

demo :: SignalFunction Stochastic UserInput Picture
demo = proc userInput -> do
  (sliderPic, r) <- slider (V2 (-400) 400) 60 -< userInput
  let std = 2 * r + 0.01
  (buttonPic, withObservation) <-
    button
      buttonParams
        { buttonPos = V2 (-300) 400,
          buttonColor = red
        }
      -<
        userInput
  (particlebuttonPic, showParticles) <-
    button
      buttonParams
        { buttonPos = V2 (-200) 400,
          buttonColor = violet
        }
      -<
        userInput
  actualPosition <- groundTruth -< userInput
  measuredPosition <- observationModel -< (actualPosition, withObservation, std)
  samples <- particleFilter params {n = 75} posterior -< (measuredPosition, withObservation, std, userInput)
  pic <-
    renderObjects
      -<
        Result
          (case measuredPosition of Just x -> x; _ -> 1000)
          actualPosition
          (if showParticles then samples else [])
  returnA
    -<
      pic <> buttonPic <> sliderPic <> particlebuttonPic
        <> translate 100 400 (scale 0.1 0.1 (text "1: Oscillator  2: Random Walk   3: White noise   4: User Input   5: Straight Lines   6: Gravity"))

agentWidth :: RealFloat a => a
agentWidth = 0.5

occlusionPrior :: SignalFunction Stochastic () Double
occlusionPrior = fmap (* 2) Example.walk1D

occlusionObsModel :: SignalFunction Deterministic (Double, (Double, Double)) (Maybe Double)
occlusionObsModel = arr \(barWidth, (agentPos, pos)) -> if abs pos < barWidth / 2 || abs (pos - agentPos) > agentWidth / 2 then Nothing else Just pos

occlusionPosterior :: SignalFunction (Stochastic & Unnormalized) (Double, Double, Maybe Double) Double
occlusionPosterior = proc (agentPos, barWidth, obs) -> do
  latent <- occlusionPrior -< ()
  predicted <- occlusionObsModel -< (barWidth, (agentPos, latent))
  case (obs, predicted) of
    (Just o, Just o') -> observe -< normalPdf o 0.1 o'
    (a, b) -> arrM condition -< a == b
  returnA -< latent

occlusion :: SignalFunction Stochastic UserInput Picture
occlusion = proc userInput -> do
  (buttonPic, withObservation) <-
    button
      buttonParams
        { buttonPos = V2 (-300) 400,
          buttonColor = red,
          buttonInitialVal = False
        }
      -<
        userInput
  let truePos = 0
  let agentPos = userInput ^. mouse . _x . to (/ 150)
  let barWidth = if withObservation then 0 else 1
  obs <- occlusionObsModel -< (into @Double barWidth, (truePos, agentPos))
  belief <- particleFilter params {n = 100} occlusionPosterior -< (agentPos, into @Double barWidth, obs)
  pic <-
    renderObjects
      -<
        Result
          { measured = case obs of Nothing -> 1000; Just o -> V2 o 2,
            particles = first (`V2` 0) <$> belief,
            latent = V2 truePos 0
          }
  let agentPosFloat = into @Float agentPos
  let trianglePic = scale 150 150 $ polygon [(agentPosFloat, 2), (agentPosFloat + agentWidth / 2, 2 + 0.25), (agentPosFloat - agentWidth / 2, 2 + 0.25), (agentPosFloat, 2)]
  returnA
    -<
      pic <> scale 150 150 (line [(- barWidth / 2, 1), (barWidth / 2, 1)]) <> buttonPic
        <> trianglePic

fullLoopPrior :: SignalFunction Stochastic Angle (V2 Double, V2 Double)
fullLoopPrior = proc ang -> do
  agentPos <- integral -< angle' ang * 0.2
  statePos <- moveAwayFrom -< agentPos
  returnA -< (4 + agentPos, statePos)

fullLoopObservationModel :: SignalFunction Stochastic (V2 Double, V2 Double) (V2 Double, V2 Double)
fullLoopObservationModel = proc (agentPos, statePos) -> do
  (x, y) <- (noise &&& noise) -< 0.5
  returnA -< (agentPos, statePos + V2 x y)
  where
    noise = arrM (normal 0)

fullLoopAgent :: SignalFunction Stochastic (V2 Double, V2 Double) (Angle, [(V2 Double, Log Double)])
fullLoopAgent = feedback (mkAngle 0) proc (obs, oldAction) -> do
  belief <- particleFilter params {n = 75} fullLoopObservationPosterior -< (oldAction, obs)
  let stateBelief = first snd <$> belief
  let agentBelief = first fst <$> belief
  let newAction = Angle $ unangle (signorm (expected stateBelief - expected agentBelief))
  returnA -< ((newAction, stateBelief), newAction)

fullLoopObservationPosterior :: SignalFunction (Stochastic & Unnormalized) (Angle, (V2 Double, V2 Double)) (V2 Double, V2 Double)
fullLoopObservationPosterior = proc (ang, (_, obs)) -> do
  latent@(_, latentState) <- fullLoopPrior -< ang
  observe -< normalPdf2D latentState 0.5 obs
  returnA -< latent

fullLoopDemo :: SignalFunction Stochastic UserInput Picture
fullLoopDemo = feedback ((0, 0), Angle 0) proc (_, (observation, action)) -> do
  trueState <- fullLoopPrior -< action
  (newAction@(Angle dir), particles) <- fullLoopAgent -< observation
  newObservation <- fullLoopObservationModel -< trueState

  pic2 <- renderObjects -< Result {particles = particles, latent = snd trueState, measured = snd newObservation}
  let pic = pic2 <> drawTriangle (fst trueState, dir, 0.2, red)
  returnA -< (pic, (newObservation, newAction))

countDemoPrior :: SignalFunction Stochastic () Position
countDemoPrior = Smoothing.prior

countDemoObservationModel :: SignalFunction Deterministic Position (V2 Int)
countDemoObservationModel = proc pos -> do
  i <- Example.edgeBy (> 0) -< pos ^. _x
  j <- Example.edgeBy (> 0) -< pos ^. _y
  returnA -< V2 i j

countDemoPosterior :: SignalFunction (Stochastic & Unnormalized) (V2 Int) Position
countDemoPosterior = proc obs -> do
  latent <- countDemoPrior -< ()
  predicted <- countDemoObservationModel -< latent
  arrM condition -< predicted == obs
  returnA -< latent

countDemoMain :: SignalFunction Stochastic UserInput Picture
countDemoMain = proc _ -> do
  truePos <- countDemoPrior -< ()
  trueObs <- countDemoObservationModel -< truePos
  belief <- particleFilter params {n = 75} countDemoPosterior -< trueObs
  pic <- renderObjects -< Result {latent = truePos, measured = 1000, particles = belief}
  returnA -< pic <> scale 0.2 0.2 (text $ show trueObs)

circlePrior :: SignalFunction Stochastic () (V2 Double)
circlePrior = proc _ -> do
  x <- Example.stochasticOscillator 0 1 -< 0.5
  y <- Example.stochasticOscillator 0 1 -< 0.5
  returnA -< V2 (abs x + 1) (abs y + 1)

circleObservationModel :: SignalFunction Stochastic (V2 Double) (V2 Double)
circleObservationModel = proc pos -> do
  (x, y) <- (noise &&& noise) -< 0.5
  returnA -< (pos + V2 x y)
  where
    noise = arrM (normal 0)

circlePosterior :: SignalFunction (Stochastic & Unnormalized) (V2 Double) (V2 Double)
circlePosterior = proc obs -> do
  latent <- circlePrior -< ()
  observe -< normalPdf2D obs 0.5 latent
  returnA -< latent

circleMain :: SignalFunction Stochastic UserInput Picture
circleMain = proc _ -> do
  true <- circlePrior -< ()
  obs <- circleObservationModel -< true
  belief <- particleFilter params {n = 75} circlePosterior -< obs
  let makePic col (V2 a b, prob) = color (withAlpha (into @Float $ exp $ 0.2 * ln prob) col) $ scale 150 150 $ scale (into @Float a) (into @Float b) (rectangleWire 2 2)
  returnA -< makePic red (true, 1.0 :: Log Double) <> mconcat (makePic violet <$> belief)
