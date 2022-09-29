{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Example
import qualified ComplexExample
import qualified Switch
import qualified Physics
-- import qualified RMSMC (gloss)
import qualified HarderObservation (gloss)
import qualified TwoStream (gloss)
-- import qualified TwoStreamContinuous (gloss)
import qualified TwoObjects (gloss)
import qualified Future
import qualified Communication
import qualified Paths
import Control.Monad
import qualified Loop
import qualified Language

main :: IO ()
main = do
    putStrLn $ "Pick an option" <>
        "\n0: Example" <>
        "\n1: Dot" <>
        "\n2: Dot tracking" <>
        "\n3: Weak prior" <>
        "\n4: No observations" <>
        "\n5: Mean changes" <>
        "\n6: Two Objects" <>
        "\n7: Two Data Streams" <>
        "\n8: Hamiltonian" <>
        "\n9: Future" <>
        "\n10: Past" <>
        "\n11: Past of filter" <>
        "\n12: Active inference" <>
        "\n13: Paths" <>
        "\n14: Restart" <>
        "\n15: Language"
    (num :: Integer) <- read <$> getLine
    case num of
        0 -> ComplexExample.gloss
        1 -> Example.dot
        2 -> Example.gloss
        3 -> Example.weakPrior
        4 -> Example.noObservations
        5 -> Switch.gloss
        6 -> TwoObjects.gloss
        7 -> HarderObservation.gloss
        8 -> Physics.gloss
        9 -> Future.future
        10 -> Future.past
        11 -> Future.pastFilter
        12 -> Communication.example
        13 -> Paths.gloss
        14 -> Loop.gloss
        15 -> Language.gloss
        _ -> void (print "no such option")


