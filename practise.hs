addInteger :: Int -> Int -> Int
addInteger a b = if a `mod` 2 == 1 || b `mod` 2 == 1 then a + b else abs (a - b)

join :: [Int] -> [Int] -> [Int]
join [] xs = xs
join (x:xs) ys = if x `notElem` ys then (x:(join xs ys)) else (join xs ys)


main :: IO()
main = print [x | x <- [1..100], x `mod` 3 == 0, x < 100]

data Tree t = Empty | Root t (Tree t) (Tree t) deriving (Show, Eq, Ord)


addNode toAdd Empty = Root toAdd Empty Empty
addNode toAdd (Root i l r) = if toAdd < i then Root i (addNode toAdd l) r else Root i l (addNode toAdd r)