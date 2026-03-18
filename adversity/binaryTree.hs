module Main where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x =
  case f x of
    Nothing -> Leaf
    Just (x1, val, x2) -> Node (unfold f x1) val (unfold f x2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  unfold
    (\m -> if m == n then Nothing else Just (m + 1, m, m + 1))
    0

main :: IO ()
main = do
  print $ treeBuild 0
  print $ treeBuild 1
  print $ treeBuild 2
  print $ treeBuild 3
