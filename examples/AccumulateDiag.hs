{-# LANGUAGE NoMonomorphismRestriction #-}
module AccumulateDiag (diag) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

-- accumulate

sampleArray :: [Int]
sampleArray = [1,2,3,4,5,6,7,8]

arrowOpts = (with & gaps .~ tiny
                  & headLength .~ Global 0.015)

arrayElementVis :: Int -> Diagram B R2
arrayElementVis n = text (show n) # fontSizeN 0.06 # font "inconsolata"
                    <> square 1 # scale 0.2

prevInputVis :: Int -> Diagram B R2
prevInputVis n = square 1 # scaleX 0.2 # scaleY 0.4 # lcA transparent
                 ===
                 text (show n) # fontSizeN 0.03 # font "inconsolata"
                 ===
                 (arrow' arrowOpts 1
                  <> square 1 # lcA transparent) # scale 0.1 # alignX 0.35

funcVis :: Diagram B R2
funcVis = text "+" # fontSizeN 0.03 # font "inconsolata"
          <> circle 1 # scale 0.05

foldElement :: Int -> Diagram B R2
foldElement n = arrayElementVis n # named "a"
                ===
                square 1 # lcA transparent # scale 0.1
                ===
                funcVis # named "f"

diagElement :: Int -> Diagram B R2
diagElement n = foldElement n
                # connectOutside' arrowOpts "a" "f"

diag :: Diagram B R2
diag = (hcat (map diagElement sampleArray) # alignX 0
       <> hcat (map prevInputVis $ scanl (+) 0 sampleArray) # alignX 0) # scaleX 0.7 # scaleY 0.7
