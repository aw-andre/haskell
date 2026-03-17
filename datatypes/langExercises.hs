module Main where

import Data.Char (toUpper)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph str = helper str True
 where
  helper [] _ = []
  helper (' ' : xs) bool = ' ' : helper xs bool
  helper ('.' : xs) _ = '.' : helper xs True
  helper (x : xs) True = toUpper x : helper xs False
  helper (x : xs) False = x : helper xs False

main :: IO ()
main = do
  print $ capitalizeWord "hello"
  print $ capitalizeParagraph "blah. woot ha."
