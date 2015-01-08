{-# LANGUAGE NoMonomorphismRestriction #-}
module MakeHeapDiag (diag) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

-- make_heap

sampleArray :: [Int]
sampleArray = [0,7,8,5,4,1,3,9,2]

heapedArray :: [Int]
heapedArray = [0,2,1,5,4,8,3,9,7]

arrayElementVis :: Int -> Diagram B R2
arrayElementVis n = text (show n) # fontSizeN 0.03 # font "inconsolata"
                    <> square 1 # scale 0.2

beforeMakeHeap :: Diagram B R2
beforeMakeHeap = hcat (map arrayElementVis sampleArray)

afterMakeHeap :: Diagram B R2
afterMakeHeap = hcat (map arrayElementVis heapedArray)

textBox :: Diagram B R2
textBox = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.4
           <> text "make_heap (greater())" # fontSizeN 0.03 # font "inconsolata") # alignX (-1.4)

makeHeapVis :: Diagram B R2
makeHeapVis = beforeMakeHeap # scale 0.4 # alignX 0 # named "before"
              ===
              textBox
              ===
              afterMakeHeap # scale 0.4 # alignX 0 # named "after"

diag :: Diagram B R2
diag = makeHeapVis # alignY 0
       # connectOutside' (with & gaps .~ small
                          & headLength .~ Global 0.04
                         ) "before" "after"
