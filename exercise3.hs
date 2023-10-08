import System.IO
import Data.List

-- Lab 3
--
-- Razvan Gorea

-- Palindromes

isPalindrome :: Eq a => [a] -> Bool
isPalindrome b = reverse b == b

-- Shortest List

shortest :: [[a]] -> [a]
shortest [] = []
shortest [x] = x
shortest (x:xs)
  | length x < length sh = x
  | otherwise = sh
  where
    sh = shortest xs

-- Adding to Polynomials

equalizeLists :: [Int] -> [Int] -> ([Int], [Int])
equalizeLists list1 list2 =
  let len1 = length list1
      len2 = length list2
      diff = abs (len1 - len2)
      list1' = if len1 < len2 then list1 ++ replicate diff 0 else list1
      list2' = if len2 < len1 then list2 ++ replicate diff 0 else list2
  in (list1', list2')

sumPoly :: [Int] -> [Int] -> [Int]
sumPoly xs ys =
  let (xs', ys') = equalizeLists xs ys
  in sumPoly' xs' ys'
  where
    sumPoly' [] [] = []
    sumPoly' (x:xs) (y:ys) = (x + y) : sumPoly' xs ys

-- Evaluating a Polynomial

test :: Int -> [Int] -> Int
test num [] = 0
test num (x:xs) =  x + num * test num xs
