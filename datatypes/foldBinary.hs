module Main where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ start Leaf = start
foldTree f start (Node left x right) =
  foldTree f (foldTree f (f x start) left) right

testTree :: BinaryTree Integer
testTree =
  Node
    (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

main :: IO ()
main = do
  print $ foldTree (+) 3 testTree
