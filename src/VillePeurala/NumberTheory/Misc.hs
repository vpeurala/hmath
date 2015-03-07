module VillePeurala.NumberTheory.Misc where

import Data.Maybe (fromJust)

positiveIntegers :: [Integer]
positiveIntegers = [1..]

newtype IncreasingIntegerSequence = IncreasingIntegerSequence (Integer -> Integer)

squareNumbers :: IncreasingIntegerSequence
squareNumbers = IncreasingIntegerSequence (\i -> i * i)

values :: IncreasingIntegerSequence -> [Integer]
values (IncreasingIntegerSequence f) = map f positiveIntegers

valuesWithNumberOfDigits :: IncreasingIntegerSequence -> Int -> [Integer]
valuesWithNumberOfDigits iis nod =
  let vs = values iis
      mn = minIntegerWithNumberOfDigits nod
      mx = maxIntegerWithNumberOfDigits nod
  in  takeWhile (<= mx) $ dropWhile (< mn) vs

maxIntegerWithNumberOfDigits :: Int -> Integer
maxIntegerWithNumberOfDigits nod = 10 ^ nod - 1

minIntegerWithNumberOfDigits :: Int -> Integer
minIntegerWithNumberOfDigits nod = 10 ^ (nod - 1)

reverseNumber :: Integer -> Integer
reverseNumber n = (signum n) * (rev (abs n) 0)
                  where
                    rev a b | a < 10 = a + b * 10
                    rev a b =
                      let q = a `quot` 10
                          r = a `rem` 10
                      in  rev q (r + b * 10)

digits :: Integer -> [Integer]
digits n = dig (abs n) []
           where
             dig n ns | n < 10 = n : ns
             dig n ns = 
               let q = n `quot` 10
                   r = n `rem` 10
               in  dig q (r : ns)

fromDigits :: [Integer] -> Integer
fromDigits ns = fdg ns 0
                where
                  fdg [] acc = acc
                  fdg (n:ns) acc = fdg ns (n + acc * 10)

numberOfDigits :: Integer -> Int
numberOfDigits = length . digits

isBijective :: (Eq a, Eq b) => [a] -> [b] -> Bool
isBijective as bs = isBijective' as bs [] && isBijective' bs as []
                    where
                      isBijective' [] [] _ = True
                      isBijective' (x:xs) [] _ = False
                      isBijective' [] (y:ys) _ = False
                      isBijective' (x:xs) (y:ys) ls =
                        let l = lookup x ls
                        in  case l of
                          Nothing -> isBijective' xs ys ((x,y):ls)
                          Just y' | y == y' -> isBijective' xs ys ls
                          Just _ -> False

data Bijection a b = Bijection [a] [b] deriving (Show)

bijection :: (Eq a, Eq b) => [a] -> [b] -> Maybe (Bijection a b)
bijection as bs = case isBijective as bs of
                    True -> Just (Bijection as bs)
                    False -> Nothing

domainToCodomain :: (Eq a, Eq b) => (Bijection a b) -> [a] -> [b]
domainToCodomain (Bijection as bs) xs =
  let l = zip as bs
  in  map (\x -> fromJust $ lookup x l) xs

isWholeNumber :: (Eq a, Num a, RealFrac a) => a -> Bool
isWholeNumber n = n == fromInteger (round n)

isSquareNumber :: Integer -> Bool
isSquareNumber n = isWholeNumber (sqrt (fromIntegral n))



