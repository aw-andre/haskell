module Main where

import Data.List (unfoldr)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case f x of
    Nothing -> []
    Just (y, z) -> y : myUnfoldr f z

betterIterate :: (a -> a) -> a -> [a]
betterIterate = myUnfoldr . (\f x -> Just (x, f x))

main :: IO ()
main = do
  print $ take 20 $ iterate (+ 1) 1
  print $ take 20 $ myIterate (+ 1) 1
  print $ take 20 $ betterIterate (+ 1) 1
  print $ take 20 $ unfoldr (\x -> if x > 10 then Nothing else Just (x, x + 1)) 1
  print $ take 20 $ myUnfoldr (\x -> if x > 10 then Nothing else Just (x, x + 1)) 1
