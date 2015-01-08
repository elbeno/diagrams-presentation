{-# LANGUAGE NoMonomorphismRestriction #-}
module AdjFindDiag (diag) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.List

-- adjacent_find

sampleArray :: [Int]
sampleArray = [0,7,8,5,4,1,3,9,2]

arrayElementVis :: Int -> Diagram B R2
arrayElementVis n = text (show n) # fontSizeN 0.03 # font "inconsolata"
                    <> square 1 # scale 0.2

arrayElementVis' :: Int -> Diagram B R2
arrayElementVis' n = text (show n) # fontSizeN 0.03 # font "inconsolata"
                    <> square 1 # scale 0.2 # bgSelect
  where bgSelect | n == 5 = fc peachpuff
                 | otherwise = fcA transparent

beforeSort :: Diagram B R2
beforeSort = hcat (map arrayElementVis sampleArray)

afterSort :: Diagram B R2
afterSort = hcat (map arrayElementVis' (sort sampleArray))

textBox :: Diagram B R2
textBox = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.2
           <> text "sort" # fontSizeN 0.03 # font "inconsolata") # alignX (-0.4)

textBox' :: Diagram B R2
textBox' = (adjFindArrow
            ===
            text "adjacent_find (b-a > 1)" # fontSizeN 0.03 # font "inconsolata")
           <> square 1 # lcA transparent # scaleX 0.4 # scaleY 0.15

arrowOpts = (with & gaps .~ tiny
                  & headLength .~ Global 0.015)

adjFindArrow = (arrow' arrowOpts 1
                <> square 1 # lcA transparent) # scale 0.1 # alignX 0.35 # rotateBy (1/4)

sortVis :: Diagram B R2
sortVis = beforeSort # scale 0.4 # alignX 0 # named "before"
          ===
          textBox
          ===
          afterSort # scale 0.4 # alignX 0 # named "after"
          ===
          textBox' # alignX (-0.4)

diag :: Diagram B R2
diag = sortVis # alignY 0
       # connectOutside' (with & gaps .~ small
                          & headLength .~ Global 0.03
                         ) "before" "after"
