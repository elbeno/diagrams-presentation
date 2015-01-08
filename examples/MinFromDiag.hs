{-# LANGUAGE NoMonomorphismRestriction #-}
module MinFromDiag (diag) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

-- minfrom


sampleBegin :: [(String, String)]
sampleBegin = [("n", "0"),("n","1"),("n","2"),
               ("...","")]

sampleEnd :: [(String, String)]
sampleEnd = [("...",""), ("n", "k-2"),("n","k-1")]

arrayElementVis :: (String, String) -> Diagram B R2
arrayElementVis (t,sub) =
  (strutX 0.1 ||| (strutY 0.1 === text sub # fontSizeN 0.015 # font "inconsolata"))
  <>
  text t # fontSizeN 0.03 # font "inconsolata"
  <> square 1 # scaleY 0.2 # scaleX 0.3 # named sub

arrayElementVis' :: (String, String) -> Diagram B R2
arrayElementVis' (t,sub) =
  (strutX 0.16 ||| (strutY 0.1 === text sub # fontSizeN 0.015 # font "inconsolata"))
  <>
  text t # fontSizeN 0.03 # font "inconsolata"
  <> square 1 # scaleY 0.2 # scaleX 0.3 # named sub

beginVis = hcat (map arrayElementVis sampleBegin)
endVis = hcat (map arrayElementVis' sampleEnd)

beforeVis =
  (beginVis
   ||| square 1 # scaleX 0.7 # scaleY 0.2
   ||| endVis)

afterVis :: Diagram B R2
afterVis = square 1 # scaleY 0.2 # scaleX 0.3
           ||| square 1 # scaleY 0.2 # scaleX 0.95
           ||| square 1 # scaleY 0.2 # scaleX 0.3 # named "p"
           ||| square 1 # scaleY 0.2 # scaleX 0.95
           ||| square 1 # scaleY 0.2 # scaleX 0.3

textBox :: Diagram B R2
textBox = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.4
           <> text "partition (0, k, < k/2)" # fontSizeN 0.02 # font "inconsolata") # alignX (-3)

arrowOpts = (with & gaps .~ tiny
                  & headLength .~ Global 0.015)

zeroPoint :: Diagram B R2
zeroPoint = (((arrow' arrowOpts 1
                <> square 1 # lcA transparent) # scale 0.15 # alignX 0.35 # rotateBy (1/4)
              ===
              text "0" # fontSizeN 0.03 # font "inconsolata")
             <> square 1 # lcA transparent # scaleX 0.4 # scaleY 0.2)

kPoint :: Diagram B R2
kPoint = (((arrow' arrowOpts 1
                <> square 1 # lcA transparent) # scale 0.15 # alignX 0.35 # rotateBy (1/4)
              ===
              text "k" # fontSizeN 0.03 # font "inconsolata")
             <> square 1 # lcA transparent # scaleX 0.4 # scaleY 0.2)

pPoint :: Diagram B R2
pPoint = (((arrow' arrowOpts 1
                <> square 1 # lcA transparent) # scale 0.15 # alignX 0.35 # rotateBy (1/4)
              ===
              text "p" # fontSizeN 0.03 # font "inconsolata")
             <> square 1 # lcA transparent # scaleX 0.4 # scaleY 0.2)

lessThanBox :: Diagram B R2
lessThanBox = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.4
               <> text "p < k/2" # fontSizeN 0.03 # font "inconsolata")

lessThanBox' :: Diagram B R2
lessThanBox' = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.2
                <> text "recurse (0, p)" # fontSizeN 0.02 # font "inconsolata")

moreThanBox :: Diagram B R2
moreThanBox = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.4
               <> text "p == k/2" # fontSizeN 0.03 # font "inconsolata")

moreThanBox' :: Diagram B R2
moreThanBox' = (square 1 # lcA transparent # scaleX 0.4 # scaleY 0.2
                <> text "recurse (p, k)" # fontSizeN 0.02 # font "inconsolata")

diag :: Diagram B R2
diag = (beforeVis # alignX 0 # named "before"
       ===
       textBox
       ===
       afterVis # alignX 0
       ===
       (zeroPoint ||| strutX 0.85 ||| pPoint ||| strutX 1.15 ||| kPoint) # centerX
       ===
       ((lessThanBox === lessThanBox') ||| strutX 1.1 ||| (moreThanBox === moreThanBox')) # alignX 0)
       # scale 0.4
       # connectOutside' (with & gaps .~ small
                          & headLength .~ Global 0.03
                         ) "before" "p"
