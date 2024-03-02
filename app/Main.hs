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

-- main = SDE.main

main :: IO ()
main = MainSF.toGloss (MainSF.mainSF [
              (GUI.gui3, "GUI", ""),
              -- (CausalInf.main, "Causal", ""),
              (Control.mainGloss, "Control", ""),
              -- (MCMC.demo1, "MCMC", ""),


              (Demo.fullLoopDemoUser, "Probabistic and reactive", ""),
              (Demo.demo, "Particle tracking", ""),
              -- --   (Concurrent.gloss, "Follow mouse", "todo"),
                (Smoothing.past, "Past smoothed", ""),
                (Smoothing.actionExample, "Past action", ""),
                (Smoothing.futureExample, "Future", ""),
              (Example.main, "Posterior predictive", ""),
              --   (Demo.occlusion, "Occlusion", ""),
              (Demo.countDemoMain, "Discrete observation", ""),
              (Pong.mainSignal, "Pong", ""),
              (Demo.fullLoopDemo, "Agent and World", ""),
              (FixedConvention.main, "Convention: fixed", ""),
              (SimpleConvention.main, "Convention: simplest", ""),
              (Convention.main, "Convention: simple", ""),
            --   (DiscreteConvention.main, "Convention", ""),
            --   (DoublyDiscreteConvention.main, "Doubly Discrete Convention", ""),
              -- (Decision.main, "Decision", ""),
              (Tutorial.demo1, "Tutorial", "")
              ])
