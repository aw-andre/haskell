module Main where

import Data.Char

upperFilter :: String -> String
upperFilter = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

allCapsRecursive :: String -> String
allCapsRecursive [] = []
allCapsRecursive (x:xs) = toUpper x : allCapsRecursive xs

headCapitalize :: String -> Char
headCapitalize = toUpper . head

main :: IO ()
main = do
  print $ upperFilter "HbEfLrLxO"
  print $ capitalize "julie"
  print $ allCapsRecursive "woot"
  print $ headCapitalize "julie"
