{-# LANGUAGE JavaScriptFFI #-}

module Canvas where

import GHC.Wasm.Prim
import Picture

newtype JSCanvas = JSCanvas JSVal

foreign import javascript unsafe "document.getElementById($1)"
  js_getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "$1.getContext('2d')"
  js_getContext2d :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.width"
  js_canvasWidth :: JSVal -> IO Double

foreign import javascript unsafe "$1.height"
  js_canvasHeight :: JSVal -> IO Double

foreign import javascript unsafe "$1.clearRect(0, 0, $2, $3)"
  js_clearRect :: JSVal -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.save()"
  js_save :: JSVal -> IO ()

foreign import javascript unsafe "$1.restore()"
  js_restore :: JSVal -> IO ()

foreign import javascript unsafe "$1.translate($2, $3)"
  js_translate :: JSVal -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.scale($2, $3)"
  js_scale :: JSVal -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.rotate($2)"
  js_rotate :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$1.beginPath()"
  js_beginPath :: JSVal -> IO ()

foreign import javascript unsafe "$1.closePath()"
  js_closePath :: JSVal -> IO ()

foreign import javascript unsafe "$1.arc(0, 0, $2, 0, 2 * Math.PI)"
  js_arc :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$1.fill()"
  js_fill :: JSVal -> IO ()

foreign import javascript unsafe "$1.stroke()"
  js_stroke :: JSVal -> IO ()

foreign import javascript unsafe "$1.fillRect($2, $3, $4, $5)"
  js_fillRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.strokeRect($2, $3, $4, $5)"
  js_strokeRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.fillStyle = $2"
  js_setFillStyle :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.strokeStyle = $2"
  js_setStrokeStyle :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.lineWidth = $2"
  js_setLineWidth :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$1.moveTo($2, $3)"
  js_moveTo :: JSVal -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.lineTo($2, $3)"
  js_lineTo :: JSVal -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.font = $2"
  js_setFont :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.fillText($2, 0, 0)"
  js_fillText :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.textAlign = 'left'"
  js_textAlignLeft :: JSVal -> IO ()

foreign import javascript unsafe "$1.textBaseline = 'alphabetic'"
  js_textBaseline :: JSVal -> IO ()

getCanvas :: String -> IO JSCanvas
getCanvas name = JSCanvas <$> js_getElementById (toJSString name)

canvasSize :: JSCanvas -> IO (Float, Float)
canvasSize (JSCanvas c) = do
  w <- js_canvasWidth c
  h <- js_canvasHeight c
  pure (realToFrac w, realToFrac h)

paintAll :: JSCanvas -> Picture -> IO ()
paintAll (JSCanvas c) pic = do
  ctx <- js_getContext2d c
  w <- js_canvasWidth c
  h <- js_canvasHeight c
  js_clearRect ctx w h
  js_save ctx
  js_translate ctx (w / 2) (h / 2)
  js_scale ctx 1 (-1)
  paint ctx pic
  js_restore ctx

paint :: JSVal -> Picture -> IO ()
paint _ Blank = pure ()
paint ctx (Circle r) = do
  js_beginPath ctx
  js_arc ctx (realToFrac r)
  js_setLineWidth ctx 2
  js_stroke ctx
paint ctx (CircleSolid r) = do
  js_beginPath ctx
  js_arc ctx (realToFrac r)
  js_fill ctx
paint _ (Line []) = pure ()
paint ctx (Line ((x0, y0) : pts)) = do
  js_beginPath ctx
  js_moveTo ctx (realToFrac x0) (realToFrac y0)
  mapM_ (\(x, y) -> js_lineTo ctx (realToFrac x) (realToFrac y)) pts
  js_setLineWidth ctx 2
  js_stroke ctx
paint _ (Polygon []) = pure ()
paint ctx (Polygon ((x0, y0) : pts)) = do
  js_beginPath ctx
  js_moveTo ctx (realToFrac x0) (realToFrac y0)
  mapM_ (\(x, y) -> js_lineTo ctx (realToFrac x) (realToFrac y)) pts
  js_closePath ctx
  js_fill ctx
paint ctx (Text s) = do
  -- Text is drawn in a y-up world; flip locally so glyphs are upright.
  js_save ctx
  js_scale ctx 1 (-1)
  js_setFont ctx (toJSString "24px sans-serif")
  js_textAlignLeft ctx
  js_textBaseline ctx
  js_fillText ctx (toJSString s)
  js_restore ctx
paint ctx (RectangleSolid w h) = do
  js_fillRect ctx (realToFrac (-w / 2)) (realToFrac (-h / 2)) (realToFrac w) (realToFrac h)
paint ctx (RectangleWire w h) = do
  js_setLineWidth ctx 2
  js_strokeRect ctx (realToFrac (-w / 2)) (realToFrac (-h / 2)) (realToFrac w) (realToFrac h)
paint ctx (Colored col p) = do
  js_setFillStyle ctx (colorToCss col)
  js_setStrokeStyle ctx (colorToCss col)
  paint ctx p
paint ctx (Translate x y p) = do
  js_save ctx
  js_translate ctx (realToFrac x) (realToFrac y)
  paint ctx p
  js_restore ctx
paint ctx (Scale sx sy p) = do
  js_save ctx
  js_scale ctx (realToFrac sx) (realToFrac sy)
  paint ctx p
  js_restore ctx
paint ctx (Rotate deg p) = do
  js_save ctx
  js_rotate ctx (realToFrac deg * pi / 180)
  paint ctx p
  js_restore ctx
paint ctx (Pictures ps) = mapM_ (paint ctx) ps

colorToCss :: Color -> JSString
colorToCss (RGBA r g b a) =
  toJSString $
    "rgba("
      <> show (round (r * 255) :: Int)
      <> ","
      <> show (round (g * 255) :: Int)
      <> ","
      <> show (round (b * 255) :: Int)
      <> ","
      <> show a
      <> ")"
