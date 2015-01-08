{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
module CopyIfDiag where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

-- copy_if

sampleArray :: [Int]
sampleArray = [3,7,8,2,1,6,5,4,4,1,3,2]

bgSelect :: (HasStyle b, V b ~ R2) => Int -> b -> b
bgSelect n | n < 5 = fc peachpuff
           | otherwise = fcA transparent

arrayElementVis :: Int -> Diagram B R2
arrayElementVis n = text (show n) # fontSizeN 0.03 # font "inconsolata"
                    <> square 1 # scale 0.2 # bgSelect n

beforeCopyIf :: Diagram B R2
beforeCopyIf = hcat (map arrayElementVis sampleArray)

afterCopyIf :: Diagram B R2
afterCopyIf = hcat (map arrayElementVis (filter (<5) sampleArray))

textBox :: Diagram B R2
textBox = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.4
           <> text "copy_if (<5)" # fontSizeN 0.03 # font "inconsolata") # alignX (-0.85)

copyIfVis :: Diagram B R2
copyIfVis = beforeCopyIf # scale 0.4 # alignX 0 # named "before"
            ===
            textBox
            ===
            afterCopyIf # scale 0.4 # alignX 0 # named "after"

diag :: Diagram B R2
diag = copyIfVis # alignY 0
       # connectOutside' (with & gaps .~ small
                          & headLength .~ Global 0.04
                         ) "before" "after"
