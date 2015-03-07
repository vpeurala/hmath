{-# LANGUAGE TupleSections #-}
module Leonhard.P98 where

import Prelude hiding (words)

import Control.Monad (forM_, when)
import qualified Data.List as L (groupBy, sort, sortBy, words)
import Data.List.Split (dropBlanks, dropDelims, oneOf, split)
import Data.Maybe (catMaybes)

import VillePeurala.NumberTheory.Misc

comparingByAsc :: (Ord b) => (a -> b) -> (a -> a -> Ordering)
comparingByAsc f = (\a1 a2 -> compare (f a1) (f a2))

comparingByDesc :: (Ord b) => (a -> b) -> (a -> a -> Ordering)
comparingByDesc f = (\a1 a2 -> compare (f a2) (f a1))

equalizingBy :: (Eq b) => (a -> b) -> (a -> a -> Bool)
equalizingBy f = (\a1 a2 -> f a1 == f a2)

isAnagramOf :: String -> String -> Bool
isAnagramOf s1 s2 = L.sort s1 == L.sort s2

findAnagrams :: [String] -> [(String, String)]
findAnagrams [] = []
findAnagrams (x:xs) =
  (map (x,) $ filter (isAnagramOf x) xs) ++ findAnagrams xs

main = do
  fileContent <- readFile "resources/p098_words.txt"
  let words = split (dropBlanks $ dropDelims $ oneOf ",\"") fileContent
  let groups = L.groupBy (equalizingBy length) $ L.sortBy (comparingByDesc length) words
  forM_ groups (\g ->
    let ans = findAnagrams g
    in  forM_ ans (\ang ->
      let (a1, a2)    = ang
          len         = length a1
          squaresWLen = valuesWithNumberOfDigits squareNumbers len
          bijections  = catMaybes (map (\n -> bijection a1 (digits n)) squaresWLen)
      in  forM_ bijections (\bject ->
        let a = fromDigits $ domainToCodomain bject a2
        in  when (isSquareNumber a) $ do
              print a2
              print a
              print a1
              print $ fromDigits $ domainToCodomain bject a1))) 

