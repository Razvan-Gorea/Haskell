
-- myAppend
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
