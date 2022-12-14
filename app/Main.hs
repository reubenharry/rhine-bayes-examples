{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Example
import qualified MainSF 
-- import qualified RMSMC (gloss)
-- import qualified TwoStreamContinuous (gloss)


main :: IO ()
main = Example.toGlossC' MainSF.mainSF

    -- putStrLn $ "Pick an option" <>
    --     "\n0: Example" <>
    --     "\n1: Dot" <>
    --     "\n2: Dot tracking" <>
    --     "\n3: Weak prior" <>
    --     "\n4: No observations" <>
    --     "\n5: Mean changes" <>
    --     "\n6: Two Objects" <>
    --     "\n7: Two Data Streams" <>
    --     "\n8: Hamiltonian" <>
    --     "\n9: Future" <>
    --     "\n10: Past" <>
    --     "\n11: Past of filter" <>
    --     "\n12: Active inference" <>
    --     "\n13: Paths" <>
    --     "\n14: Restart" <>
    --     "\n15: Language" <>
    --     "\n16: Predictive" <> 
    --     "\n17: Beta Bernoulli" <>
    --     "\n18: Ising Model" <>
    --     "\n19: Split" <> 
    --     "\n20: Pong" <> 
    --     "\n21: Pong Example" <> 
    --     "\n22: Control" <> 
    --     "\n23: Interaction" <>
    --     "\n24: Lazy" <> 
    --     "\n25: Circular"
    -- (num :: Integer) <- read <$> getLine
    -- case num of
    --     -- 1 -> Example.toGloss Example.dot
    --     2 -> Example.toGlossC' Example.gloss
    --     -- 3 -> Example.toGloss Example.weakPrior
    --     -- 4 -> Example.toGloss Example.noObservations
    --     -- 5 -> Switch.gloss
    --     6 -> TwoObjects.gloss
    --     -- 7 -> HarderObservation.gloss
    --     8 -> Physics.gloss
    --     -- 9 -> Future.future
    --     -- 10 -> Future.past
    --     -- 11 -> Future.pastFilter
    --     -- 12 -> Active.visualizer
    --     -- 13 -> Paths.gloss
    --     -- 14 -> Loop.gloss
    --     15 -> Language.gloss
    --     -- 16 -> Example.toGloss Example.predictive
    --     -- 17 -> BetaBern.gloss
    --     18 -> Ising.gloss
    --     19 -> Split.gloss
    --     -- 20 -> Pong.gloss
    --     -- 21 -> PongExample.gloss
    --     -- 22 -> Control.gloss
    --     23 -> GlossInput.gloss
    --     24 -> Lazy.gloss
    --     26 -> JSON.gloss
    --     1 -> Example.toGlossC' MainSF.mainSF
    --     _ -> void (print "no such option")


