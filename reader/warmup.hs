module Main where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
-- tupled = (,) <$> cap <*> rev
tupled = cap >>= \capped -> rev >>= \revved -> return (capped, revved)

main :: IO ()
main = do
  print $ tupled "hello"
