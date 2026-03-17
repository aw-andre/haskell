module Main where

import Data.Char (toUpper)

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xfull@(x : xs) (y : ys) = if x == y then isSubseqOf xs ys else isSubseqOf xfull ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = (map capitalTuple) . words
 where
  capitalTuple :: String -> (String, String)
  capitalTuple s@(x : xs) = (s, toUpper x : xs)
  capitalTuple _ = error "Bad arg"

main :: IO ()
main = do
  print $ isSubseqOf "blah" "balah"
  print $ isSubseqOf "blah" "baalh"
  print $ capitalizeWords "hello world"
  print $ capitalizeWords ""
