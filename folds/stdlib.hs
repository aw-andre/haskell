module Main where

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny = (or .) . map

myElem :: (Eq a) => a -> [a] -> Bool
myElem = any . (==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = (squish .) . map

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

order :: Ordering -> (a -> a -> Ordering) -> a -> a -> a
order ord f x y = if (f x y) == ord then x else y


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = foldr1 . order GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = foldr1 . order LT

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

main :: IO ()
main = do
  print $ myOr [False, False]
  print $ myOr [False, True]
  print $ myAny even [1, 3, 5]
  print $ myAny even [1, 2, 5]
  print $ myElem 3 [1, 2, 3]
  print $ myElem 3 [1, 2, 4]
  print $ myReverse [1, 2, 3]
  print $ squish [[1, 2], [3, 4], [5]]
  print $ squishMap (\x -> map (+ 1) x) [[1, 2], [3, 4], [5]]
  print $ squishAgain [[1, 2], [3, 4], [5]]
  print $ myMaximum [1, 2, 3]
  print $ myMinimum [1, 2, 3]
