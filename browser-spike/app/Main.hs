{-# LANGUAGE JavaScriptFFI #-}

module Main where

import BayesLite (SamplerIO, sampleIOwith)
import Canvas
import Data.Automaton (Automaton, stepAutomaton)
import Data.Automaton.Trans.Reader (runReaderS)
import Data.IORef
import Data.Stream.Result (Result (..))
import GHC.Wasm.Prim
import Picture
import RhineCompat
import System.IO.Unsafe (unsafePerformIO)
import System.Random (StdGen, mkStdGen)
import Tutorial (demo1)
import UserInput

-- | demo1 with TimeInfo supplied explicitly each frame.
type DemoSF = Automaton SamplerIO (TimeInfo DtClock, UserInput) Picture

{-# NOINLINE simRef #-}
simRef :: IORef DemoSF
simRef = unsafePerformIO (newIORef (runReaderS demo1))

-- | Persist RNG across frames. Resetting it each step freezes the noise
-- process (and thus the integrated latent) at a fixed trajectory point.
{-# NOINLINE genRef #-}
genRef :: IORef StdGen
genRef = unsafePerformIO (newIORef (mkStdGen 42))

{-# NOINLINE inputRef #-}
inputRef :: IORef UserInput
inputRef = unsafePerformIO (newIORef noInput)

{-# NOINLINE lastTimeRef #-}
lastTimeRef :: IORef Double
lastTimeRef = unsafePerformIO (newIORef 0)

{-# NOINLINE sinceInitRef #-}
sinceInitRef :: IORef Float
sinceInitRef = unsafePerformIO (newIORef 0)

{-# NOINLINE frameCbRef #-}
frameCbRef :: IORef JSVal
frameCbRef = unsafePerformIO (newIORef (error "frame callback not initialised"))

foreign import javascript "wrapper"
  makeFrameCallback :: (Double -> IO ()) -> IO JSVal

foreign import javascript "wrapper"
  makeMouseCallback :: (Double -> Double -> IO ()) -> IO JSVal

foreign import javascript unsafe "window.requestAnimationFrame($1)"
  js_requestAnimationFrame :: JSVal -> IO ()

foreign import javascript unsafe "document.getElementById($1).addEventListener('mousemove', function(e){ var r = e.target.getBoundingClientRect(); $2(e.clientX - r.left, e.clientY - r.top); })"
  js_onMouseMove :: JSString -> JSVal -> IO ()

foreign import javascript unsafe "console.log($1)"
  js_log :: JSString -> IO ()

startDemo :: IO ()
startDemo = do
  js_log (toJSString "browser-spike: Tutorial.demo1")
  mouseCb <- makeMouseCallback onMouse
  js_onMouseMove (toJSString "sim") mouseCb
  frameCb <- makeFrameCallback onFrame
  writeIORef frameCbRef frameCb
  js_requestAnimationFrame frameCb

onMouse :: Double -> Double -> IO ()
onMouse canvasX canvasY = do
  canvas <- getCanvas "sim"
  (w, h) <- canvasSize canvas
  let x = realToFrac canvasX - w / 2
      y = h / 2 - realToFrac canvasY
  modifyIORef' inputRef $ \ui -> ui {mouseX = x, mouseY = y}

onFrame :: Double -> IO ()
onFrame tMs = do
  t0 <- readIORef lastTimeRef
  writeIORef lastTimeRef tMs
  let dt
        | t0 == 0 = 1 / 30
        | otherwise = min 0.05 ((tMs - t0) / 1000)
  modifyIORef' sinceInitRef (+ realToFrac dt)
  tAbs <- readIORef sinceInitRef
  ui <- readIORef inputRef
  msf <- readIORef simRef
  g <- readIORef genRef
  let ti = mkTimeInfo tAbs (realToFrac dt)
  (Result msf' pic, g') <- sampleIOwith g $ stepAutomaton msf (ti, ui)
  writeIORef simRef msf'
  writeIORef genRef g'
  canvas <- getCanvas "sim"
  paintAll canvas pic
  frameCb <- readIORef frameCbRef
  js_requestAnimationFrame frameCb

foreign export ccall "hs_startDemo" startDemo :: IO ()

main :: IO ()
main = pure ()
