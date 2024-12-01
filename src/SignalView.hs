{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SignalView where
import Concurrent (UserInput)
import Util
import FRP.Rhine.Gloss
import Control.Category ((.))
import Prelude hiding ((.))
import Data.Sequence (Seq, viewr, ViewR (EmptyR, (:>)))
import Control.Monad.Bayes.Class
import Data.Foldable (Foldable(toList), fold)
import Witch (into)
import Smoothing (delayBy', historySince', delayBy'')
import Linear (V2(..))
import GUI (slider)
import Debug.Trace (traceM)


{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}

func :: a >--> Double
func = proc _ -> do
  time <- sinceInitS -< ()
  returnA -< sin (time * 2 * pi)

func2 :: Double >--> Double
func2 = proc d -> do
    x <- func -< ()
    delayBy' 2 -< (x, d)



-- foo :: (MonadDistribution m, TimeDomain (Time cl), Time cl ~ Double, Diff (Time cl) ~ Double) => ClSF m cl () Picture 
foo :: Color -> Double >--> Picture 
foo col = arr (scale 40 40) . arr fst .  arr (foldr (\(ti, fx) (pic, accum) -> let new = (into @Float (sinceLast ti)) in (pic <> translate accum (into @Float fx) (color col $ circleSolid 0.1), accum+new) ) (mempty, 0) ) . arr toList . historySince 10

sf :: UserInput >--> Picture
sf = proc userInput -> do
    (sliderPic, radius) <- slider (V2 (-300) 300) 60 -< userInput
    pic1 <- foo red . func -< ()
    x <- func -< ()
    te <- sinceInitS -< ()
    pic2 <- foo green . func2 -< (sin(te))
    -- bar <- delayBy' 2 -< (x, (100 * radius))
    arrM traceM -< show $ te/10
    -- arrM traceM -< show $ radius
    -- baz <- historySince 2 -< (x)
    -- arrM traceM -< show $ safeHead $ viewr $ snd <$> baz

    returnA -< pic1 <> pic2  <> sliderPic



    where
        safeHead EmptyR = Nothing
        safeHead (_ :> a) = Just a