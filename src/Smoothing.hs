module Smoothing where

import Concurrent (UserInput)
import Control.Monad.Bayes.Class (MonadDistribution (normal, uniformD), normalPdf)
import Data.MonadicStreamFunction
import Example
  ( Observation,
    Position,
    Result (Result, latent, measured, particles),
    renderObjects,
    stochasticOscillator, empirical
  )
import FRP.Rhine.Gloss.Common (Picture)
import GHC.Float ()
import GUI (slider)
import Inference
import Linear (V2 (..))
import Util
import Control.Category
import Prelude hiding (id, (.))
import FRP.Rhine (integral, Rhine, ClSF, TimeDomain (Diff, diffTime), Clock (Time), TimeInfo (absolute), readerS, BehaviorF, lastS, historySince)
import qualified Control.Category as C
import qualified Example
import Data.Sequence (Seq, takeWhileL, (<|), empty, ViewR (EmptyR, (:>)), viewr)
import qualified FRP.Rhine.ClSF.Reader as Q
import qualified Control.Monad.Trans.MSF as D


pastExample :: SignalFunction Stochastic UserInput Picture
pastExample = proc _ -> do

  actualPosition <- prior -< ()
  measuredPosition <- observationModel -< actualPosition
  samples <- particleFilter params (shift 50 . posterior) -< measuredPosition


  renderObjects
      -<
        Result
          { particles = samples,
            measured = 1000,
            latent = actualPosition
          }

futureExample :: SignalFunction Stochastic UserInput Picture
futureExample = proc userInput -> do

  (sliderPic, r) <- slider (V2 (-400) 300) 60 -< userInput
  actualPosition <- prior -< ()
  measuredPosition <- observationModel -< actualPosition
  samples <- particleFilter params (posterior')  -< (measuredPosition, floor $ r * 100 + 1)


  pic <- renderObjects
      -<
        Result
          { particles = samples,
            measured = 1000,
            latent = actualPosition
          }
  returnA -< pic <> sliderPic


past :: SignalFunction Stochastic UserInput Picture
past = proc userInput -> do
  (sliderPic, r) <- slider (V2 (-400) 300) 60 -< userInput
  actualPosition <- prior -< ()
  thePast <- shift 50 -< actualPosition
  measuredPosition <- observationModel -< actualPosition
  samples <- particleFilter params (posterior *** returnA >>> shiftBy) -< (measuredPosition, floor $ r * 100 + 1)
  pic <-
    renderObjects
      -<
        Result
          { particles = samples,
            measured = thePast,
            latent = actualPosition
          }
  pic2 <-
    renderObjects
      -<
        Result
          { particles = [],
            measured = measuredPosition,
            latent = 1000
          }
  returnA -< pic <> sliderPic <> pic2


actionExample :: SignalFunction Stochastic UserInput Picture
actionExample = proc _ -> do

  (action, pos) <- constantly empirical . particleFilter params full -< ()
  -- agentPosition <- integral -< action

  out <- renderObjects
      -<
        Result
          { particles = [],
            measured = V2 action 0,
            latent = pos
          }

  returnA -< out

std :: Double
std = 0.5

prior :: SignalFunction Stochastic () Position
prior = proc () -> do
  x <- stochasticOscillator 0 2 -< 1
  y <- stochasticOscillator 2 0 -< 1
  returnA -< V2 x y


posterior :: (Observation) >-/-> Position
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- prior -< ()
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

posterior' :: (Observation, Int) >-/-> Position
posterior' = proc (V2 oX oY, a) -> do
  latent <- prior -< ()
  pastLatent@(V2 trueX trueY) <- shiftBy -< (latent, a)
  observe -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent

observationModel :: SignalFunction Stochastic Position Observation
observationModel = proc p -> do
  (x, y) <- (noise &&& noise) -< ()
  returnA -< p + V2 x y
  where
    noise = constM (normal 0 0.1)

type Action = Double

agent :: Double >-/-> Action
agent = decision

decision :: Double >-/-> Action
decision = proc pos -> do
  action <- constM $ uniformD [-1,1] -< ()
  observe -< (normalPdf 3 1) pos
  returnA -< action

fullPrior :: () >--> (Action, Position)
fullPrior = feedback (0,0) proc ((), (oldAction, oldPos)) -> do
  oldAction' <- shift 50 -< oldAction
  newPos <- integral -< V2 oldAction' 0
  newAction <- constM $ uniformD [-1,1] -< ()
  let out = (newAction, newPos)
  returnA -< (out, out)

full :: () >-/-> (Action, Position)
full = proc () -> do
  ap@(action, position) <- fullPrior -< ()
  observe -< normalPdf2D 3 1 position
  returnA -< ap


shift :: Monad m => Int -> MSF m c c
shift n = accumulateWith (\x xs -> take n $ x : xs) [] >>> arr last

-- shiftBy :: Monad m => Int -> MSF m c c
shiftBy :: Monad m => MSF m (c, Int) c
shiftBy = accumulateWith (\(x, n) xs -> take n $ x : xs) [] >>> arr last

shiftBy' :: (Monad m, Diff (Time cl) ~ a) => ClSF m cl (c, a) c
shiftBy' = accumulateWith undefined undefined >>> arr last

historySince' ::
  (Monad m, Ord (Diff (Time cl)), TimeDomain (Time cl)) =>
  -- | The size of the time window
  
  ClSF m cl (a, Diff (Time cl)) (Seq (TimeInfo cl, a))
historySince' = D.readerS $ accumulateWith appendValue empty
  where
      appendValue :: (Ord (Diff (Time cl)), TimeDomain (Time cl)) => (TimeInfo cl, (a, Diff (Time cl))) -> Seq (TimeInfo cl, a) -> Seq (TimeInfo cl, a)
      appendValue (ti, (a, dTime)) tias = takeWhileL (recentlySince dTime ti) $ (ti, a) <| tias
    -- appendValue (ti, a) tias = takeWhileL (recentlySince ti) $ (ti, a) <| tias
      recentlySince dTime ti (ti', _) = diffTime (absolute ti) (absolute ti') < dTime

delayBy' ::
  (Monad m, Ord (Diff td), TimeDomain td) =>
  -- | The time span to delay the signal
  a ->
  BehaviorF m td (a, Diff td) a
delayBy' a' = arr (safeHead . viewr) . arr (fmap snd) . historySince' 
  where
    safeHead EmptyR = a'
    safeHead (_ :> a) = a

delayBy'' ::
  (Monad m, Ord (Diff td), TimeDomain td) =>
  -- | The time span to delay the signal
  a ->
  Diff td -> 
  BehaviorF m td a a
delayBy'' a' difft = historySince difft  >>> arr (fmap snd) >>> arr (viewr >>> safeHead)

  where
      safeHead EmptyR = a'
      safeHead (_ :> a) = a

