import System.IO

-- Lab 2
--
-- Area of a Triangle

s :: Float -> Float -> Float -> Float
s a b c = (a + b + c) / 2

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
  if a + b > c && b + c > a && c + a > b
    then sqrt (s a b c * (s a b c - a) * (s a b c - b) * (s a b c - c))
    else error "Not a Triangle!"

-- Sum Test

isSum :: Int -> Int -> Int -> Bool
isSum x y z = x + y == z
