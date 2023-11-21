-- Tree Data Structure
-- Author: Razvan Gorea
-- A 2-3 Tree can either be empty(nil), a 2-node(1 root and two children) , or a 3-node(two roots and three children) 
data Tree t = 
  Nil
  | Node2 t (Tree t) (Tree t)
  | Node3 t t (Tree t) (Tree t) (Tree t) deriving (Eq, Show, Ord)


-- Add function that adds a node to the 2-3 tree
-- If the tree is empty, then the node is added as the root (in this case, the root is a 2-node)
-- Otherwise it adds the node as another root, the 2 roots then create three children nil nodes
-- Future nodes then are added to the tree by comparing the node to the roots, deteriming its position in the tree, and the number of children nodes
add :: Ord t => t -> Tree t -> Tree t
add a Nil = Node2 a Nil Nil
add x (Node2 a left right)  
   | x <= a = Node3 x a Nil Nil Nil
   | otherwise = Node3 a x Nil Nil Nil
add x (Node3 a b left middle right)
   | x <= a = Node3 a b (add x left) middle right
   | x > a && x <= b = Node3 a b left (add x middle) right
   | otherwise = Node3 a b left middle (add x right)


-- Member function that checks if a node is in the 2-3 tree
-- The member function recursively goes through the tree, left most as possible, until it finds the node
member :: (Ord t) => t -> Tree t -> Bool
member _ Nil = False
member x (Node2 a left right)
   | x == a = True
   | x <= a = member x left
   | otherwise = member x right
member x (Node3 a b left middle right)
   | x == a || x == b = True
   | x <= a = member x left
   | x > a && x <= b = member x middle
   | otherwise = member x right


-- Height function that returns the height of the 2-3 tree
height :: Tree t -> Int
height Nil = -1
height (Node2 _ left right) = 1 + max (height left) (height right)
height (Node3 _ _ left middle right) = 1 + max (height left) (max(height middle) (height right))


-- makeTree function that takes a list and returns a 2-3 tree
-- The makeTree function recursively goes through a given list, extracting the head through each pass and adding the head to the tree
-- The add function is used to add the head to the tree
makeTree :: Ord t => [t] -> Tree t
makeTree [] = Nil
makeTree (x:xs) = add x (makeTree xs)


-- Unsure if correct???
prettyPrint :: Show t => Tree t -> String
prettyPrint Nil = ""
prettyPrint (Node2 a left right) = prettyPrint left ++ show a ++ prettyPrint right
prettyPrint (Node3 a b left middle right) = prettyPrint left ++ show a ++ prettyPrint middle ++ show b ++ prettyPrint right


main :: IO()
main = 
   do
      let tree = add 3 Nil
      let tree1 = add 2 tree
      let tree2 = add 1 tree1
      let tree3 = add 4 tree2
      print(prettyPrint tree3)
{-

Questions:

1. What happens when you use the add method to add a node to an empty tree?
2. If the height method is presented with an empty tree, should it return 0, -1 or 1?
3. How to pretty print?
4. Add Method? is less that or equal to correct? or should it just be less than?

-}