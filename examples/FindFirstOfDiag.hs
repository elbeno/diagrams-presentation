{-# LANGUAGE NoMonomorphismRestriction #-}
module FindFirstOfDiag (diag) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

-- find_first_of

arrowOpts = (with & gaps .~ tiny
                  & headLength .~ Global 0.015)

findSet :: [Int]
findSet = [1,2,3,4,5]

sampleArray :: [Int]
sampleArray = [6,8,11,3,4,7,8,4,1,5,5,2,10,9]

findElementVis :: Int -> Diagram B R2
findElementVis n = text (show n) # fontSizeN 0.03 # font "inconsolata"
                    <> square 1 # scale 0.2

arrayElementVis :: Int -> Diagram B R2
arrayElementVis n = text (show n) # fontSizeN 0.03 # font "inconsolata"
                    <> square 1 # scale 0.2 # bgSelect
  where bgSelect | n == 3 || n == 2 = fc peachpuff
                 | otherwise = fcA transparent

setVis :: Diagram B R2
setVis = hcat (map findElementVis findSet)

arrayVis :: Diagram B R2
arrayVis = hcat (map arrayElementVis sampleArray)

textBox :: Diagram B R2
textBox = (((arrow' arrowOpts 1
                <> square 1 # lcA transparent) # scale 0.1 # alignX 0.35 # rotateBy (1/4)
               ===
               text "find_first_of" # fontSizeN 0.03 # font "inconsolata")
              <> square 1 # lcA transparent # scaleX 0.4 # scaleY 0.2)

textBoxRev :: Diagram B R2
textBoxRev = (((arrow' arrowOpts 1
                <> square 1 # lcA transparent) # scale 0.1 # alignX 0.35 # rotateBy (1/4)
               ===
               text "find_first_of" # fontSizeN 0.03 # font "inconsolata"
               ===
               square 1 # lcA transparent # scaleX 0.05 # scaleY 0.05
               ===
               text "(reverse_iterator)" # fontSizeN 0.03 # font "inconsolata")
              <> square 1 # lcA transparent # scaleX 0.4 # scaleY 0.2)

findFirstOfVis :: Diagram B R2
findFirstOfVis = setVis # scale 0.4 # alignX 0
                 ===
                 square 1 # lcA transparent # scaleY 0.2
                 ===
                 arrayVis # scale 0.4 # alignX 0
                 ===
                 (textBox
                  |||
                  square 1 # lcA transparent # scaleX 0.235 # scaleY 0.2
                  |||
                  textBoxRev) # alignX (-0.085)

diag :: Diagram B R2
diag = findFirstOfVis # alignY 0
       # connectOutside' (with & gaps .~ small
                          & headLength .~ Global 0.04
                         ) "before" "after"
