module Main where

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)

main :: IO ()
main = do
  print $ myZipWith (+) [1..10] [1..20]
  print $ myZip [1..10] ['a'..'z']
