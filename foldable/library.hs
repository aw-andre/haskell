module Main where

import Data.Maybe (fromJust)

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (+) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\y -> (x == y ||)) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (\x y -> if y == Nothing || x < fromJust y then Just x else y) Nothing

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr (\x y -> if x > fromJust y then Just x else y) Nothing

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ n -> n + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = Prelude.foldMap id

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (\x y -> f x <> y) mempty

main :: IO ()
main = do
  print $ Main.null []
