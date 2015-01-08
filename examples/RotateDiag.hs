{-# LANGUAGE NoMonomorphismRestriction #-}
module RotateDiag (diag) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.List

-- rotate

sampleRot :: [Int]
sampleRot = [1,2,3,4,5]

arrayElementVis :: Int -> Diagram B R2
arrayElementVis n = text (show n)  # fontSizeN 0.03 # font "inconsolata"
                    <> square 1 # scaleY 0.2 # fc peachpuff

arrayElementVisNamed :: Int -> Diagram B R2
arrayElementVisNamed n = arrayElementVis n # named n

arrayMiddleVis :: Bool -> [Int] -> Diagram B R2
arrayMiddleVis b l = vcat (map (if b then arrayElementVisNamed else arrayElementVis) l)

emptyElementVis :: Diagram B R2
emptyElementVis = square 1 # scaleY 0.2 # fc white

arrayTopVis :: Diagram B R2
arrayTopVis = vcat (replicate 2 emptyElementVis)

arrayBottomVis :: Diagram B R2
arrayBottomVis = vcat (replicate 3 emptyElementVis)

arrayVis :: Bool -> [Int] -> Diagram B R2
arrayVis b l = arrayTopVis
               ===
               arrayMiddleVis b l
               ===
               arrayBottomVis


textBox :: Diagram B R2
textBox = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.5
           <> text "rotate" # fontSizeN 0.03 # font "inconsolata") # alignY (-0.2)

arrowOpts = (with & arrowShaft .~ arcshaft
             & gaps .~ small
             & headLength .~ Global 0.02
            )

rotateVis :: Diagram B R2
rotateVis = arrow' arrowOpts 1 # rotateBy (-1/4) # alignY (-0.2) # scaleY 0.27
            |||
            arrayVis True sampleRot # scale 0.3 # alignY 0 # named "before"
            |||
            textBox
            |||
            arrayVis False sampleRot' # scale 0.3 # alignY 0 # alignX (-0.2) # named "after"
  where sampleRot' = tail sampleRot ++ [head sampleRot]

arcshaft = arc (0 @@ turn) (1/8 @@ turn)

diag :: Diagram B R2
diag = rotateVis # alignX 0
       # connectOutside' (with & gaps .~ small
                          & headLength .~ Global 0.04
                         ) "before" "after"
