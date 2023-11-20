data Tree t = 
  Nil
  | Node2 t (Tree t) (Tree t)
  | Node3 t t (Tree t) (Tree t) (Tree t) deriving (Eq, Show, Ord)

add :: Ord t => t -> Tree t -> Tree t
add a Nil = Node2 a Nil Nil
add x (Node2 a left right)  
   | x <= a = Node2 a (add x left) right 
   | otherwise = Node2 a left (add x right)
add x (Node3 a b left middle right)
   | x <= a = Node3 a b (add x left) middle right
   | x > a && x < b = Node3 a b left (add x middle) right
   | otherwise = Node3 a b left middle (add x right)


member :: (Ord t) => t -> Tree t -> Bool
member _ Nil = False
member x (Node2 a left right)
   | x == a = True
   | x <= a = member x left
   | otherwise = member x right
member x (Node3 a b left middle right)
   | x == a || x == b = True
   | x <= a = member x left
   | x > a && x < b = member x middle
   | otherwise = member x right

