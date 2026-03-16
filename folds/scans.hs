module Main where

factorials :: [Integer]
factorials = scanl (*) 1 [1..]

main :: IO ()
main = do
  print $ take 10 factorials
