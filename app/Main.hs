{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified MainSF 

main :: IO ()
main = MainSF.toGloss MainSF.mainSF
