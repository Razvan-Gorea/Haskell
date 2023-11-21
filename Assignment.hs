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
-- Adding a node to an empty 2-3 Tree
add a Nil = Node2 a Nil Nil
add x (Node2 a left right) 
-- If the node is less than or equal to the root node, then the node is added to the left of the root node
   | x <= a = Node3 x a Nil Nil Nil
-- Else add to the right of the root node
   | otherwise = Node3 a x Nil Nil Nil
add x (Node3 a b left middle right)
-- If the node is less than or equal to the first root node, then the node is added to the left of the 2 root nodes
   | x <= a = Node3 a b (add x left) middle right
-- If the node is greater than the first root node and less than or equal to the second root node, then the node is added to the middle of the 2 root nodes
   | x > a && x <= b = Node3 a b left (add x middle) right
-- Else the node is added to the right of the 2 root nodes
   | otherwise = Node3 a b left middle (add x right)


-- Member function that checks if a node is in the 2-3 tree
-- The member function recursively goes through the tree, left most as possible, until it finds the node
member :: (Ord t) => t -> Tree t -> Bool
-- If the tree is empty, the node can't be in the tree, hence false
member _ Nil = False
member x (Node2 a left right)
-- If the node is equal to the root node, then the node is in the tree
   | x == a = True
-- If the node is less than the root node, then check the left side of the tree for the node
   | x <= a = member x left
-- Else check the right side of the tree for the node
   | otherwise = member x right
member x (Node3 a b left middle right)
-- If the node is equal to any of the 2 root nodes, then the node is in the tree
   | x == a || x == b = True
-- If the node is less than the first root node, then check the left side of the tree for the node
   | x <= a = member x left
-- If the node is greater than the first root node and less than or equal to the second root node, then check the middle side of the tree for the node
   | x > a && x <= b = member x middle
-- Else check the right side of the tree for the node
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


-- My Implementation of a pretty print function
-- Not the best, since I couldn't figure out how to format the way I wanted to.
-- The problem occured when I tried to change one niche aspect of the formatt, it would break other things in the process
-- I Might look into solving it in the future
prettyPrint :: Show t => Tree t -> String
prettyPrint tree = prettyPrint' tree 0

prettyPrint' :: Show t => Tree t -> Int -> String
prettyPrint' Nil _ = "Nil"
prettyPrint' (Node2 a left right) count = replicate count ' ' ++ show a ++ " [" ++ prettyPrint' left (count + 2) ++ ", " ++ prettyPrint' right (count + 2) ++ "]"
prettyPrint' (Node3 a b left middle right) count = replicate count ' ' ++ show a ++ "," ++ show b ++ "\n" ++ replicate (count + 4) ' ' ++ "|---->" ++ prettyPrint' left (count + 0) ++ "\n" ++ replicate (count + 4) ' ' ++ "|---->" ++ prettyPrint' middle (count + 6) ++ "\n" ++ replicate (count + 4) ' ' ++ "|---->" ++ prettyPrint' right (count + 6)

{-
Comment the following section in to test add, prettyPrint, member and height functions
A 2-3 tree called tree7 is created by adding a bunch of numbers repeatedly to the tree, over and over again

-}

{-
main :: IO()
main = 
   do
      let tree = add 3 Nil
      let tree1 = add 2 tree
      let tree2 = add 1 tree1
      let tree3 = add 4 tree2
      let tree4 = add 5 tree3
      let tree5 = add 6 tree4
      let tree6 = add 7 tree5
      let tree7 = add 8 tree6
      print(member 3 tree7)
      print(member 9 tree7)
      print(height tree7)
      putStrLn(prettyPrint tree7)
-}