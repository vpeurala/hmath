module Leonhard.P102 where

import Control.Monad (forM_)

import Data.List (splitAt)
import Data.List.Split (splitOn)

import VillePeurala.Geometry.Geometry2D (Triangle, barycentricCoordinates, mkPoint, mkTriangle, origin, triangleContainsOrigin)

parseTriangle :: String -> Triangle
parseTriangle s =
  let nss      = splitOn "," s
      ns       = map read nss
      (p1, ps) = splitAt 2 ns
      (p2, p3) = splitAt 2 ps
      a        = mkPoint (head p1) (last p1)
      b        = mkPoint (head p2) (last p2)
      c        = mkPoint (head p3) (last p3)
  in  mkTriangle a b c

main = do
  fileContent <- readFile "resources/p102_triangles.txt"
  let triangles = map parseTriangle $ lines fileContent
  let barycentrics = map (\t -> barycentricCoordinates t origin) triangles
  forM_ triangles (\t ->
    let barycentrics = barycentricCoordinates t origin
        containsOrigin = triangleContainsOrigin t
    in  putStrLn $ (show t) ++ " " ++ (show barycentrics) ++ " -> " ++ (show containsOrigin))
  let trianglesContainingOrigin = filter triangleContainsOrigin triangles
  print $ length trianglesContainingOrigin

