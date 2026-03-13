module Main where

import Data.Char

caesar :: Int -> String -> String
caesar n xs = map (shift n) xs
 where
  aint = ord 'a'
  shift :: Int -> Char -> Char
  shift m c = chr $ (+aint) $ (ord c - aint + m) `mod` 26

uncaesar :: Int -> String -> String
uncaesar = caesar . negate

main :: IO ()
main = do
  print $ uncaesar 27 $ caesar 27 "abcdefg"
