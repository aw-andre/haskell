module Main where

import Data.Char (chr, ord)

type Code = String

caesar :: Int -> String -> String
caesar n xs = map (shift n) xs
 where
  aint = ord 'a'
  shift :: Int -> Char -> Char
  shift m c = chr $ (+ aint) $ (ord c - aint + m) `mod` 26

uncaesar :: Int -> String -> String
uncaesar = caesar . negate

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
  putStr "Which cipher do you want: "
  choice <- getLine
  if choice `elem` ["caesar", "uncaesar", "vigenere", "unvigenere"]
    then do
      putStr "Enter shift: "
      shift <- getLine
      putStr "Enter message: "
      message <- getLine
      putStrLn $ case choice of
        "caesar" -> caesar (read shift) message
        "uncaesar" -> uncaesar (read shift) message
        "vigenere" -> encrypt shift message
        "unvigenere" -> decrypt shift message
        _ -> error "unreachable"
    else putStrLn "Invalid choice!"
