

module Deterministic where
    
import Inference
import Util
import Data.Text (Text)
import FRP.Rhine.Gloss.Common (Picture)
import Control.Arrow (returnA)
import Example ( drawBall', edgeBy)
import Linear (V2(..))
import Graphics.Gloss ( Color, text )
import FRP.Rhine.Gloss ( red, blue, yellow, translate )
import Data.MonadicStreamFunction (MSF)

oscillator :: SignalFunction Deterministic () Double
oscillator = proc () -> do
    t <- time -< ()
    returnA -< sin t

main :: SignalFunction Deterministic Text Picture
main = proc inputText -> do
    position <- oscillator -< ()
    color <- hold red -< toColor inputText
    counter <- edgeBy (>0.01) -< position
    picture <- renderObjects -< (position, color, counter)
    returnA -< picture


toColor :: Text -> Maybe Color
toColor = \case
    "red" -> Just red
    "blue" -> Just blue
    "yellow" -> Just yellow
    _ -> Nothing


renderObjects ::  Monad m => MSF m (Double, Color, Int) Picture
renderObjects = proc (pos, color, i) -> do

  ball <- drawBall' -< (V2 pos 0, 0.3, color)
  returnA -< (ball <> translate 0 100 (text (show i)))
