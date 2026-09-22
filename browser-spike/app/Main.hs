{-# LANGUAGE JavaScriptFFI #-}

module Main where

import Canvas
import BayesLite (SamplerIO, sampleIO)
import Control.Monad.Trans.MSF.Reader (ReaderT)
import Data.IORef
import Data.MonadicStreamFunction (MSF)
import qualified Data.Set as Set
import GHC.Wasm.Prim
import Picture
import RhineCompat
import System.IO.Unsafe (unsafePerformIO)
import Tracker (tracker)
import UserInput

type TrackerSF = MSF (ReaderT (TimeInfo DtClock) SamplerIO) UserInput Picture

{-# NOINLINE simRef #-}
simRef :: IORef TrackerSF
simRef = unsafePerformIO (newIORef tracker)

{-# NOINLINE inputRef #-}
inputRef :: IORef UserInput
inputRef = unsafePerformIO (newIORef noInput)

{-# NOINLINE lastTimeRef #-}
lastTimeRef :: IORef Double
lastTimeRef = unsafePerformIO (newIORef 0)

{-# NOINLINE sinceInitRef #-}
sinceInitRef :: IORef Double
sinceInitRef = unsafePerformIO (newIORef 0)

{-# NOINLINE frameCbRef #-}
frameCbRef :: IORef JSVal
frameCbRef = unsafePerformIO (newIORef (error "frame callback not initialised"))

foreign import javascript "wrapper"
  makeFrameCallback :: (Double -> IO ()) -> IO JSVal

foreign import javascript "wrapper"
  makeMouseCallback :: (Double -> Double -> IO ()) -> IO JSVal

foreign import javascript "wrapper"
  makeButtonCallback :: (JSString -> Bool -> IO ()) -> IO JSVal

foreign import javascript unsafe "window.requestAnimationFrame($1)"
  js_requestAnimationFrame :: JSVal -> IO ()

foreign import javascript unsafe "document.getElementById($1).addEventListener('mousemove', function(e){ var r = e.target.getBoundingClientRect(); $2(e.clientX - r.left, e.clientY - r.top); })"
  js_onMouseMove :: JSString -> JSVal -> IO ()

foreign import javascript unsafe "document.getElementById($1).addEventListener('click', function(){ $2($1, true); setTimeout(function(){ $2($1, false); }, 150); })"
  js_onButtonClick :: JSString -> JSVal -> IO ()

foreign import javascript unsafe "console.log($1)"
  js_log :: JSString -> IO ()

startDemo :: IO ()
startDemo = do
  js_log (toJSString "browser-spike: particle tracker (lite)")
  mouseCb <- makeMouseCallback onMouse
  js_onMouseMove (toJSString "sim") mouseCb
  btnCb <- makeButtonCallback onButton
  js_onButtonClick (toJSString "pulse") btnCb
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

onButton :: JSString -> Bool -> IO ()
onButton name down = do
  let n = fromJSString name
  modifyIORef' inputRef $ \ui ->
    ui
      { buttonsDown =
          if down
            then Set.insert n (buttonsDown ui)
            else Set.delete n (buttonsDown ui)
      }

onFrame :: Double -> IO ()
onFrame tMs = do
  t0 <- readIORef lastTimeRef
  writeIORef lastTimeRef tMs
  let dt
        | t0 == 0 = 1 / 30
        | otherwise = min 0.05 ((tMs - t0) / 1000)
  modifyIORef' sinceInitRef (+ dt)
  tAbs <- readIORef sinceInitRef
  ui <- readIORef inputRef
  msf <- readIORef simRef
  let ti = mkTimeInfo tAbs dt
  (pic, msf') <- sampleIO $ stepClSF msf ti ui
  writeIORef simRef msf'
  canvas <- getCanvas "sim"
  paintAll canvas pic
  frameCb <- readIORef frameCbRef
  js_requestAnimationFrame frameCb

foreign export ccall "hs_startDemo" startDemo :: IO ()

main :: IO ()
main = pure ()
