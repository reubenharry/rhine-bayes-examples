module Example where
import Control.Monad.Bayes.Population
    ( resampleMultinomial )
import Data.MonadicStreamFunction
    ( returnA, (>>>), arrM, constM, Arrow(first, arr, (&&&)), withSideEffect_ )
import Control.Monad.Bayes.Class
    ( MonadSample(normal), factor, normalPdf, MonadInfer )
import FRP.Rhine
    ( VectorSpace((*^)),
      arrMCl,
      average,
      liftClock,
      reactimateCl,
      LiftClock, TimeDomain (Diff), BehaviourF )
import GHC.Float (float2Double, double2Float)
import FRP.Rhine.Gloss
    ( blue,
      green,
      red,
      withAlpha,
      circleSolid,
      color,
      scale,
      translate,
      Display(InWindow),
      defaultSettings,
      clearIO,
      launchGlossThread,
      paintIO,
      GlossSettings(display),
      GlossConcT,
      GlossSimClockIO(..), Event (EventKey), Key (Char), KeyState (Down) )
import Control.Monad.Bayes.Sampler ( sampleIO, SamplerIO )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import qualified Data.Vector.Sized as V
import Numeric.Hamilton ()
import Numeric.LinearAlgebra.Static ()
import Control.Monad.Trans.Identity ( IdentityT(runIdentityT) )
import Inference (pattern V2, xCoord, yCoord, NormalizedDistribution, StochasticSignal, StochasticSignalTransform, UnnormalizedDistribution, averageOf, stdDevOf, onlineSMC)

