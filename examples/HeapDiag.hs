{-# LANGUAGE NoMonomorphismRestriction #-}
module HeapDiag (diag) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

-- heap visualization
heapedArray :: [Int]
heapedArray = [0,2,1,5,4,8,3,9,7]

arrayElementVis :: Int -> Diagram B R2
arrayElementVis n = text (show n) # fontSizeN 0.03 # font "inconsolata"
                    <> square 1 # scale 0.2 # named (show n)

afterMakeHeap :: Diagram B R2
afterMakeHeap = hcat (map arrayElementVis heapedArray)

makeHeapVis :: Diagram B R2
makeHeapVis =  afterMakeHeap # scale 0.4 # alignX 0

heapArray :: Diagram B R2
heapArray = makeHeapVis # alignY 0
            # connectPerim' arrowOpts "0" "1" (1/4 @@ turn) (1/4 @@ turn)
            # connectPerim' arrowOpts "0" "2" (1/4 @@ turn) (1/4 @@ turn)
            # connectPerim' arrowOpts "1" "8" (1/4 @@ turn) (1/4 @@ turn)
            # connectPerim' arrowOpts "1" "3" (1/4 @@ turn) (1/4 @@ turn)
            # connectPerim' arrowOpts' "2" "5" (3/4 @@ turn) (3/4 @@ turn)
            # connectPerim' arrowOpts' "2" "4" (3/4 @@ turn) (3/4 @@ turn)
            # connectPerim' arrowOpts' "5" "9" (3/4 @@ turn) (3/4 @@ turn)
            # connectPerim' arrowOpts' "5" "7" (3/4 @@ turn) (3/4 @@ turn)

arrowOpts = (with & arrowShaft .~ arcshaft
             & headLength .~ Global 0.02
            )

arrowOpts' = (with & arrowShaft .~ arcshaft'
              & headLength .~ Global 0.02
             )

arcshaft = arc' (-1) (1/4 @@ turn) (0 @@ turn)
arcshaft' = arc (0 @@ turn) (1/4 @@ turn)

heapElementVis :: Int -> Diagram B R2
heapElementVis n = text (show n) # fontSizeN 0.03 # font "inconsolata"
                    <> circle 0.2 # scale 0.2 # named (show n)

heapLayer1 = heapElementVis 0

heapLayer2 = heapElementVis 2
             ||| strutX 0.25
             ||| heapElementVis 1

heapLayer3 = heapElementVis 5
             ||| strutX 0.1
             ||| heapElementVis 4
             ||| strutX 0.1
             ||| heapElementVis 8
             ||| strutX 0.1
             ||| heapElementVis 3

heapLayer4 = heapElementVis 9
             ||| strutX 0.05
             ||| heapElementVis 7


heapVis :: Diagram B R2
heapVis = heapLayer1 # centerX
          ===
          strutY 0.05
          ===
          heapLayer2 # centerX
          ===
          strutY 0.05
          ===
          heapLayer3 # centerX
          ===
          strutY 0.05
          ===
          heapLayer4 # alignX 2.6

arrowOpts'' = (with  -- & gaps .~ small
               & headLength .~ Global 0.02
              )

heapTree = heapVis
           # connectOutside' arrowOpts'' "0" "2"
           # connectOutside' arrowOpts'' "0" "1"
           # connectOutside' arrowOpts'' "1" "8"
           # connectOutside' arrowOpts'' "1" "3"
           # connectOutside' arrowOpts'' "2" "5"
           # connectOutside' arrowOpts'' "2" "4"
           # connectOutside' arrowOpts'' "5" "9"
           # connectOutside' arrowOpts'' "5" "7"

diag = heapArray
       ===
       strutY 0.1
       ===
       heapTree
