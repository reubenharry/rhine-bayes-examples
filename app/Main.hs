{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



import qualified MainSF 

import qualified Demo
import qualified GUI
import qualified Smoothing
import qualified Example
import qualified DiscreteConvention
import qualified DoublyDiscreteConvention
import qualified Decision
import qualified Tutorial
import qualified Convention


main :: IO ()
main = MainSF.toGloss (MainSF.mainSF [
            --   (GUI.gui, "GUI", ""),
              (Demo.demo, "Particle tracking", ""),
              (Convention.main, "Convention: simple", ""),
            -- --   (Concurrent.gloss, "Follow mouse", "todo"),
            --   (Smoothing.past, "Past smoothed", ""),
            --   (Example.main, "Posterior predictive", ""),
            --   (Demo.occlusion, "Occlusion", ""),
            --   (Demo.countDemoMain, "Discrete observation", ""),
            --   (Demo.fullLoopDemo, "Agent and World", ""),
            --   (DiscreteConvention.main, "Convention", ""),
            --   (DoublyDiscreteConvention.main, "Doubly Discrete Convention", ""),
            --   (Decision.main, "Decision", ""),
              (Tutorial.demo1, "Tutorial", "")
              ])
