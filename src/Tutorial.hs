module Tutorial where

import Inference
import FRP.Rhine.Gloss
import Control.Monad.Bayes.Class (MonadSample(..))
import Util



walk1D :: SignalFunction Stochastic () Double
walk1D = proc _ -> do
    dacceleration <- constM (normal 0 8 ) -< ()
    acceleration <- integral -< dacceleration
    velocity <- integral -< acceleration -- Integral, dying off exponentially
    position <- integral -< velocity
    returnA -< position

