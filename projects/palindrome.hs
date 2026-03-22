module Main where

import Control.Monad
import Data.Char
import System.Exit

isPalindrome :: String -> Bool
isPalindrome str = filtstr == reverse filtstr
 where
  filtstr = filter (`elem` "abcdefghijklmnopqrstuvwxyz") $ map toLower str

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case isPalindrome line1 of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

main :: IO ()
main = do
  palindrome
