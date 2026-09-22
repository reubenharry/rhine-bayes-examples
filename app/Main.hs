{-# LANGUAGE Arrows #-}

module Main (main) where

import qualified MainSF
import qualified Tutorial

main :: IO ()
main =
  MainSF.toGloss $
    proc (ui, _) -> do
      Tutorial.demo1 -< ui
