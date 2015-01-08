{-# LANGUAGE NoMonomorphismRestriction #-}
module PartitionDiag (diag) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.List

-- partition

selector :: [(a,a) -> a]
selector = [fst, snd, snd, fst, fst, snd, fst, fst, snd, fst]

beforePartition :: Colour Double -> Colour Double -> [Colour Double]
beforePartition a b = selector <*> [(a, b)]

afterPartition :: Colour Double -> Colour Double -> [Colour Double]
afterPartition a b = a' ++ b'
  where (a',b') = partition (==a) (beforePartition a b)

arrayElementVis :: Colour Double -> Diagram B R2
arrayElementVis c = square 1 # scaleY 0.2 # fc c

arrayVis :: [Colour Double] -> Diagram B R2
arrayVis l = vcat (map arrayElementVis l)

textBox :: Diagram B R2
textBox = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.5
           <> text "" # fontSizeN 0.03 # font "inconsolata") # alignY (-0.2)

partitionVis :: Diagram B R2
partitionVis = arrayVis (beforePartition peachpuff powderblue) # scale 0.3
               # alignY 0 # named "before"
               |||
               textBox
               |||
               arrayVis (afterPartition peachpuff powderblue) # scale 0.3
               # alignY 0 # alignX (-0.2) # named "after"

diag :: Diagram B R2
diag = partitionVis # alignX 0
       # connectOutside' (with & gaps .~ small
                          & headLength .~ Global 0.04
                         ) "before" "after"
