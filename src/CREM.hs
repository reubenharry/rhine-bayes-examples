import Crem.Render.Render (renderUntypedGraph, machineAsGraph, Mermaid, baseMachineAsGraph, renderGraph)
import Crem.StateMachine
import Control.Monad.Bayes.Enumerator
import Control.Arrow (returnA, (|||), Arrow ((***)))
import Data.Functor.Identity
import Crem.BaseMachine (BaseMachineT, identity)
import Crem.Render.RenderFlow
import qualified Control.Category as C


foo :: Monad m => StateMachineT m Double Double
foo = proc x -> do
    y <- stateless (+1) -< x
    z <- statelessT (return) -< y
    returnA -< y

-- bar :: Mermaid
bar = renderFlow @Identity (BinaryLabel (LeafLabel "foo") (LeafLabel "bar")) foo 

baz = renderFlow
          @Identity
          (BinaryLabel (LeafLabel "foo") (BinaryLabel (LeafLabel "bar") (LeafLabel "baz")))
          ( 
              (stateless $ (\x -> [x :: Int]) ) `Feedback`
              (proc x -> do  
                a <- (stateless id)  -< x 
                (stateless (\x -> [x])) -< a
                ) :: StateMachineT Identity Int [Int])