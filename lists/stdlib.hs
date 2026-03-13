module Main where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = or . map f

myElem :: (Eq a) => a -> [a] -> Bool
myElem x = any $ (==) x

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaxMin :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myMaxMin _ _ [] = error "Empty list"
myMaxMin ord f (x : xs) = helper ord f xs x
 where
  helper _ _ [] acc = acc
  helper ord f (x : xs) acc =
    if f acc x == ord then acc else x

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myMaxMin GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myMaxMin LT

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
  print $ squishMap (\x -> map (+1) x) [[1, 2], [3, 4], [5]]
  print $ squishAgain [[1, 2], [3, 4], [5]]
  print $ myMaximum [1, 2, 3]
  print $ myMinimum [1, 2, 3]
