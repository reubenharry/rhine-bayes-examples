{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}

module Main (main) where



import qualified MainSF 

import qualified Demo
import qualified GUI
import qualified Smoothing
import qualified Example
-- import qualified DiscreteConvention
import qualified DoublyDiscreteConvention
import qualified Decision
import qualified Tutorial
import qualified Convention
import qualified Pong
import qualified CausalInf
import qualified Control
import qualified MCMC
import qualified SimpleConvention
import qualified FixedConvention
import qualified SDE
import Control.Arrow (returnA)
import qualified DiscreteLanguageConvention
import qualified SignalView


-- main = MainSF.main2

main :: IO ()
main = MainSF.toGloss (MainSF.mainSF [
              (GUI.gui3, "GUI", ""),
              -- (CausalInf.main, "Causal", ""),
              (Control.mainGloss, "Control", ""),
              (SDE.sf, "SDE", ""),
              (SDE.sf2, "SDE particle tracking marginalized", ""),
              (SDE.sf3, "SDE particle tracking", ""),
              (SDE.sf4, "banana", ""),
              -- (, "MainSF 2", ""),
              -- (MCMC.demo1, "MCMC", ""),


              (Demo.fullLoopDemoUser, "Probabistic and reactive", ""),
              (Demo.demo, "Particle tracking", ""),
              -- --   (Concurrent.gloss, "Follow mouse", "todo"),
                (Smoothing.past, "Past smoothed", ""),
                (Smoothing.actionExample, "Past action", ""),
                -- (Smoothing.futureExample, "Future", ""),
              (SignalView.sf, "Signal View", ""),
              (Example.main, "Posterior predictive", ""),
              --   (Demo.occlusion, "Occlusion", ""),
              (Demo.countDemoMain, "Discrete observation", ""),
              (Pong.mainSignal, "Pong", ""),
              (Demo.fullLoopDemo, "Agent and World", ""),
              (FixedConvention.main, "Convention: fixed", ""),
              (SimpleConvention.main, "Convention: simplest", ""),
              (Convention.main, "Convention: simple", ""),
              (DiscreteLanguageConvention.main, "Convention: discrete language", ""),
            --   (DiscreteConvention.main, "Convention", ""),
            --   (DoublyDiscreteConvention.main, "Doubly Discrete Convention", ""),
              -- (Decision.main, "Decision", ""),
              (Tutorial.demo1, "Tutorial", "")
              ])
