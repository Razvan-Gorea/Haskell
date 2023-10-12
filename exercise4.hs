
-- myAppend
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Distribution.Simple.Setup (falseArg)
{-# HLINT ignore "Use foldr" #-}

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

-- myHead

myHead :: [a] -> a
myHead (x:xs) = x

-- myLast

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- myTail

myTail :: [a] -> [a]
myTail [] = []
myTail (x:xs) = xs

-- myInit

myInit :: [a] -> [a]
myInit [] = []
myInit [x] = []
myInit (x:xs) = x : myInit xs

-- myLength

myLength  :: [a] -> Int
myLength [] = 0
myLength (x:xs) = myLength xs + 1

-- myReverse

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myConcat :: [[a]] -> [a]
myConcat [[]] = []
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = mySum xs + x

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = myProduct xs * x

myMaximum :: Ord a => [a] -> a
myMaximum [a] = a
myMaximum (x:xs) = myMaximum xs

myMinimum :: Ord a => [a] -> a
myMinimum [a] = a
myMinimum (x:xs) = x

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = 
    if x == y then True else myElem x ys