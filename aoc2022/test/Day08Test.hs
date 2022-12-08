
module Day08Test where
import Test.HUnit
import Day08
import qualified Data.Matrix as M

day08Tests = [
    "takeWhile1" ~: "all" ~: takeWhile1 (<=10) [1..5] ~?= [1,2,3,4,5]
    , "takeWhile1" ~: "none" ~: takeWhile1 (<=0) [1..5] ~?= [1]      
    , "takeWhile1" ~: "plus one" ~: takeWhile1 (<=3) [1..5] ~?= [1,2,3,4]
    , "getTargetIndexes" ~: "square" ~: getTargetIndexes (2,2) (M.zero 3 3) ~?= [[(1,2)], [(2,3)], [(3,2)], [(2,1)]]
  , "getTargetIndexes" ~: "square edge" ~: getTargetIndexes (1,3) (M.zero 3 3) ~?= [[], [], [(2,3), (3,3)], [(1,2), (1,1)]]
  , "getTargetIndexes" ~: "rect" ~: getTargetIndexes (2,2) (M.zero 3 4) ~?= [[(1,2)], [(2,3), (2,4)], [(3,2)], [(2,1)]]
  , "getTargetIndexes" ~: "rect edge" ~: getTargetIndexes (1,1) (M.zero 4 3) ~?= [[], [(1,2), (1,3)], [(2,1),(3,1), (4,1)], []]
  , "scenicScore" ~: "zeros" ~: scenicScore (M.zero 3 3) (2,2) ~?= 1
  , "scenicScore" ~: "non zeros" ~: scenicScore (M.fromLists [[0,0,0],[0,1,0],[0,0,0]]) (2,2) ~?= 1
  ]
