{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import qualified PartitionDiag as P
import qualified CopyIfDiag as CI
import qualified ReplaceIfDiag as RI
import qualified TransformDiag as T
import qualified UniqueCopyDiag as UC
import qualified AccumulateDiag as Acc
import qualified FindFirstOfDiag as FFO
import qualified RotateDiag as R
import qualified HeapDiag as H
import qualified MakeHeapDiag as MH
import qualified AdjFindDiag as AF
import qualified MinFromDiag as MF

page :: Diagram B R2
page = square 1 # scaleX (14.0/9.0) # fc white # lcA transparent

--main :: IO ()
--main = mainWith $ diag # centerXY <> page

main :: IO ()
main = multiMain [("partition", P.diag # centerXY <> page),
                  ("copy_if", CI.diag # centerXY <> page),
                  ("replace_if", RI.diag # centerXY <> page),
                  ("transform", T.diag # centerXY <> page),
                  ("unique_copy", UC.diag # centerXY <> page),
                  ("accumulate", Acc.diag # centerXY <> page),
                  ("find_firstof", FFO.diag # centerXY <> page),
                  ("rotate", R.diag # centerXY <> page),
                  ("make_heap", MH.diag # centerXY <> page),
                  ("heap", H.diag # centerXY <> page),
                  ("adjacent_find", AF.diag # centerXY <> page),
                  ("min_from", MF.diag # centerXY <> page)
                  ]
