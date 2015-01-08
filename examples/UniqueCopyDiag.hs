{-# LANGUAGE NoMonomorphismRestriction #-}

module UniqueCopyDiag (diag) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.List

-- unique_copy

sampleArray :: [Int]
sampleArray = [3,7,4,4,5,1,1,1,2,7,5,3,3,4,2]

sampleArrayColour = concatMap selectColour $ map length $ group sampleArray
  where selectColour 1 = [fcA transparent]
        selectColour n = replicate n (fc peachpuff)

sampleArrayColour' = map (selectColour . length) $ group sampleArray
  where selectColour 1 = fcA transparent
        selectColour _ = fc peachpuff

arrayElementVis (n, c) = text (show n) # fontSizeN 0.03 # font "inconsolata"
                         <> square 1 # scale 0.2 # c

beforeUniqueCopy :: Diagram B R2
beforeUniqueCopy = hcat (zipWith (curry arrayElementVis) sampleArray sampleArrayColour)

afterUniqueCopy :: Diagram B R2
afterUniqueCopy = hcat (zipWith (curry arrayElementVis)
                        (map head $ group sampleArray) sampleArrayColour')

textBox :: Diagram B R2
textBox = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.4
           <> text "unique_copy" # fontSizeN 0.03 # font "inconsolata") # alignX (-0.8)

uniqueCopyVis :: Diagram B R2
uniqueCopyVis = beforeUniqueCopy # scale 0.4 # alignX 0 # named "before"
                ===
                textBox
                ===
                afterUniqueCopy # scale 0.4 # alignX 0 # named "after"

diag :: Diagram B R2
diag = uniqueCopyVis # alignY 0
       # connectOutside' (with & gaps .~ small
                          & headLength .~ Global 0.04
                         ) "before" "after"
