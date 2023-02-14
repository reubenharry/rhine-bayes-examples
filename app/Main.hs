{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



import qualified MainSF 

import qualified Demo
import qualified GUI
import qualified Concurrent
import qualified Smoothing
import qualified Example
import qualified Tutorial
import qualified DiscreteConvention


main :: IO ()
main = MainSF.toGloss (MainSF.mainSF [
              (GUI.gui, "GUI", ""),
              (Demo.demo, "Particle tracking", ""),
            --   (Concurrent.gloss, "Follow mouse", "todo"),
              (Smoothing.past, "Past smoothed", ""),
              (Example.main, "Posterior predictive", ""),
              (Demo.occlusion, "Occlusion", ""),
              (Demo.countDemoMain, "Discrete observation", ""),
              (Demo.fullLoopDemo, "Agent and World", ""),
              (DiscreteConvention.main, "Convention", "")
              ])
