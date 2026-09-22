{-# LANGUAGE Arrows #-}

-- | Toy mouse-follow demo kept for smoke-testing the canvas path.
module Demo where

import Control.Arrow (arr, returnA)
import Data.MonadicStreamFunction (MSF, feedback)
import Picture
import UserInput

type DemoSF = MSF IO (Float, UserInput) Picture

demo :: DemoSF
demo = proc (dt, ui) -> do
  (x, y) <- feedback (0, 0) (arr stepPos) -< ui
  pulse <- feedback 0 (arr stepPulse) -< (dt, ui)
  let radius = 20 + 30 * pulse
      col = if buttonDown "pulse" ui then yellow else red
      pic =
        translate x y $
          color col $
            circleSolid radius
  returnA -< pic
  where
    stepPos (ui', _)
      | buttonDown "reset" ui' = ((0, 0), (0, 0))
      | otherwise =
          let p = (mouseX ui', mouseY ui')
           in (p, p)

    stepPulse ((dt', ui'), t)
      | buttonDown "pulse" ui' = (1, 1)
      | otherwise =
          let t' = max 0 (t - dt')
           in (t', t')
