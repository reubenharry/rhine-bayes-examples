module Control where

import FRP.Rhine (integral, returnA)
import Util
import Concurrent
import FRP.Rhine.Gloss
import Example (drawBall, empirical, drawParticle, decayingIntegral)
import Linear (V2(..))
import Control.Monad.Bayes.Class
import Inference (particleFilter, params, SMCSettings (n))
import Control.Category ((.))
import Prelude hiding ((.))
import Debug.Trace (traceM)
import Data.Foldable (Foldable(fold))
import Control.Monad.Trans.MSF.List (mapMSF)
import Decision (stepper)
import qualified Smoothing
import Data.MonadicStreamFunction.InternalCore (MSF(MSF))
import Control.Monad.Bayes.Enumerator (expectation)
import qualified Data.Vector as V

type Position = Double

type Action = Double

system :: System Deterministic Action Position
system = integral

brownianMotion1D :: SignalFunction Stochastic () Double
brownianMotion1D = proc _ -> do
  dacceleration <- constM (normal 0 16) -< ()
  acceleration <- decayingIntegral 1 -< dacceleration
  velocity <- decayingIntegral 1 -< acceleration -- Integral, dying off exponentially
  position <- decayingIntegral 1 -< velocity
  returnA -< position



systemUnlooped :: System Deterministic (Action, Position) (Position, Position)
systemUnlooped = proc (a, p) -> do
    returnA -< let newp = p+0.01*a in (newp, newp)

agent :: System (Stochastic & Unnormalized) Position Action
agent = proc pos -> do
    action <- brownianMotion1D -< ()
    -- constM $ uniformD [-1, 1] -< ()
    (newPos, msf) <- stepper (feedback 0 systemUnlooped) -< pos
    next <- arrM id -< runNSteps msf 1 action
    -- olderAction <- Smoothing.shift 100 -< action
    -- newPos <- feedback 0 systemUnlooped -< olderAction
    -- altNeuPos <- Smoothing.shift 100 -< newPos
    -- futurePos :: Double <- arrM id -< runNSteps steps 5 action
    let mass = ( Exp $ log $ 1/((next - 2)**2))
    observe -< mass
    arrM traceM -< show  (mass, newPos, pos)
    returnA -< (action)

mainGloss :: System Stochastic UserInput Picture
mainGloss = feedback (-1) proc (_, oldAction) -> do
    pos <- feedback 0 systemUnlooped -< oldAction
    newActions <- particleFilter params{n=100} agent -< pos
    -- newAction <- arr (\x -> sum x / fromIntegral (length x)) -< fst <$> newActions
    newAction <- arrM empirical -< newActions
    pic <- drawBall -< (V2 pos 0, 0.2, red)
    pic3 <- drawBall -< (V2 2 0, 0.1, red)
    pic2 <- fold <$> mapMSF drawParticle -< first (`V2` 0) <$> newActions
    returnA -< (pic <> pic2 <> pic3, newAction)
