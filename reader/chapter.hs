module Main where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs = lookup 3 $ zip x y
ys = lookup 6 $ zip y z
zs = lookup 4 $ zip x z
z' = (`lookup` zip x z)

x1 = (,) <$> xs <*> ys
x2 = (,) <$> ys <*> zs
x3 = (,) <$> z' <*> z'

summed = uncurry (+)

bolt = (&&) <$> (> 3) <*> (< 8)

sequA = sequenceA [(>3), (<8), even]
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ foldr (&&) True $ sequA 7
  print $ sequA <$> s'
  print $ bolt <$> ys
