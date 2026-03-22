module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "Invalid digit"

digits :: Int -> [Int]
digits 0 = [0]
digits n = reverse $ helper n
 where
  helper :: Int -> [Int]
  helper 0 = []
  helper n = (n `mod` 10) : helper (n `div` 10)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n

main :: IO ()
main = do
  print $ wordNumber 12345 -- "one-two-three-four-five"
  print $ wordNumber 0     -- "zero"
  print $ wordNumber 9876543210 -- "nine-eight-seven-six-five-four-three-two-one-zero"
