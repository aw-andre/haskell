module Main where

import Data.Char (chr, ord)

type Code = String

extend :: Int -> Code -> Code
extend num code =
  if length code == 0
    then []
    else foldr (++) [] $ take num $ repeat code

toInts :: Code -> [Int]
toInts = map ord

encode :: (Int -> Int -> Int) -> Code -> String -> String
encode f code str =
  zipWith
    addInt
    str
    $ toInts
    $ extend (length str) code
  where
    addInt :: Char -> Int -> Char
    addInt dec n = if dec == ' ' then ' ' else chr $ mod (f (ord dec - ord 'a') n) 26 + ord 'a'

encrypt :: Code -> String -> String
encrypt = encode (+)

decrypt :: Code -> String -> String
decrypt = encode (-)

main :: IO ()
main = do
  print $ encrypt "ally" "we are going to war"
  print $ decrypt "ally" $ encrypt "ally" "we are going to war"
