module VillePeurala.Geometry.Geometry2D where

data Point = Point Double Double deriving (Show)

mkPoint :: Double -> Double -> Point
mkPoint x y = Point x y

data Triangle = Triangle Point Point Point deriving (Show)

mkTriangle :: Point -> Point -> Point -> Triangle
mkTriangle a b c = Triangle a b c

data BarycentricCoordinates = BarycentricCoordinates Double Double Double deriving (Show)

barycentricCoordinates :: Triangle -> Point -> BarycentricCoordinates
barycentricCoordinates (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) (Point x y) =
  let detT = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
      num1 = (y2 - y3) * (x - x3) + (x3 - x2) * (y - y3)
      num2 = (y3 - y1) * (x - x3) + (x1 - x3) * (y - y3)
      b1   = num1 / detT
      b2   = num2 / detT
      b3   = 1 - b1 - b2
  in  BarycentricCoordinates b1 b2 b3

triangleContainsPoint :: Triangle -> Point -> Bool
triangleContainsPoint triangle point =
  let (BarycentricCoordinates b1 b2 b3) = barycentricCoordinates triangle point
  in  all (\b -> b >= 0 && b <= 1) [b1, b2, b3]

origin :: Point
origin = mkPoint 0 0

triangleContainsOrigin :: Triangle -> Bool
triangleContainsOrigin triangle = triangleContainsPoint triangle origin


