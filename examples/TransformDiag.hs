{-# LANGUAGE NoMonomorphismRestriction #-}
module TransformDiag (diag) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

-- transform

sampleArray :: [Int]
sampleArray = [3,7,8,2,1,6,3,4,4,1,3,2]

arrayElementVis :: Int -> Diagram B R2
arrayElementVis n = text (show n) # fontSizeN 0.03 # font "inconsolata"
                    <> square 1 # scale 0.2

beforeTransform :: Diagram B R2
beforeTransform = hcat (map arrayElementVis sampleArray)

afterTransform :: Diagram B R2
afterTransform = hcat (map (arrayElementVis . (*2)) sampleArray)

textBox :: Diagram B R2
textBox = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.4
           <> text "transform (*2)" # fontSizeN 0.03 # font "inconsolata") # alignX (-1)

transformVis :: Diagram B R2
transformVis = beforeTransform # scale 0.4 # alignX 0 # named "before"
               ===
               textBox
               ===
               afterTransform # scale 0.4 # alignX 0 # named "after"

diag :: Diagram B R2
diag = transformVis # alignY 0
       # connectOutside' (with & gaps .~ small
                          & headLength .~ Global 0.04
                         ) "before" "after"
