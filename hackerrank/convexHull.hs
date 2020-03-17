module ConvexHull where

import Data.List

data Point = Point Int Int deriving (Show, Eq, Ord)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) =
  sqrt $ fromIntegral (y2 - y1) ^ 2 + fromIntegral (x2 - x1) ^ 2


angle :: Point -> Point -> Double
angle (Point x1 y1) (Point x2 y2)
  | dx == 0   = if y2 > y1 then pi else -pi
  | otherwise = atan (dy/dx)
  where
    dy = fromIntegral y2 - fromIntegral y1
    dx = fromIntegral x2 - fromIntegral x1


orderAngleDistance :: Point -> Point -> Point -> Ordering
orderAngleDistance sp p1 p2
  | a1 < a2  = LT
  | a1 > a2  = GT
  | a1 == a2 = compare d1 d2
  where
    a1 = angle sp p1
    a2 = angle sp p2
    d1 = distance sp p1
    d2 = distance sp p2


isRightTurn :: Point -> Point -> Point -> Bool
isRightTurn (Point x1 y1) (Point x2 y2) (Point x3 y3) = (x2 - x1)*(y3-y1) - (y2-y1)*(x3-x1) < 0

dropWhileRightTurn :: Point -> [Point] -> [Point]
dropWhileRightTurn p [] = [p]
dropWhileRightTurn p (p1:[]) = [p, p1]
dropWhileRightTurn p (p1:p2:pts) =
  if isRightTurn p p1 p2 then
    dropWhileRightTurn p (p2:pts)
  else
    (p:p1:p2:pts)
  


convexHull :: [Point] -> [Point]
convexHull points = hull
  where
    sP = minimum points
    sortedPoints = sortBy (orderAngleDistance sP) points
    hull = foldr dropWhileRightTurn [] sortedPoints


testPoints :: [Point]
testPoints = [Point 0 0, Point 5 5, Point 0 5, Point 2 0, Point 4 2, Point 5 0]
