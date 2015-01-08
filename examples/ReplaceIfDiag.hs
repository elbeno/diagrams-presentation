{-# LANGUAGE NoMonomorphismRestriction #-}
module ReplaceIfDiag (diag) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

-- replace_if

sampleArray :: [Int]
sampleArray = [3,7,8,2,1,6,3,4,4,1,3,2]

arrayElementVis :: Int -> Diagram B R2
arrayElementVis n = text (show n) # fontSizeN 0.03 # font "inconsolata"
                    <> square 1 # scale 0.2 # bgSelect
  where bgSelect | n >= 5 = fc peachpuff
                 | otherwise = fcA transparent

beforeReplaceIf :: Diagram B R2
beforeReplaceIf = hcat (map arrayElementVis sampleArray)

afterReplaceIf :: Diagram B R2
afterReplaceIf = hcat (map (arrayElementVis . (\x -> if x > 5 then 5 else x)) sampleArray)

textBox :: Diagram B R2
textBox = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.4
           <> text "replace_if (>5) 5" # fontSizeN 0.03 # font "inconsolata") # alignX (-1.2)

replaceIfVis :: Diagram B R2
replaceIfVis = beforeReplaceIf # scale 0.4 # alignX 0 # named "before"
            ===
            textBox
            ===
            afterReplaceIf # scale 0.4 # alignX 0 # named "after"

diag :: Diagram B R2
diag = replaceIfVis # alignY 0
       # connectOutside' (with & gaps .~ small
                          & headLength .~ Global 0.04
                         ) "before" "after"
