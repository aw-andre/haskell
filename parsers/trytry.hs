module Main where

import Control.Applicative
import Data.Ratio
import Text.Trifecta

fraction :: Parser Rational
fraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero."
    _ -> return (numerator % denominator)

digits :: Integer -> Integer
digits 0 = 0
digits x = 1 + (digits $ x `div` 10)

dec :: Parser Rational
dec = do
  integer <- decimal
  char '.'
  float <- decimal
  let places = 10 ^ digits float
  return $ (integer * places + float) % places

fracOrDec :: Parser Rational
fracOrDec = try dec <|> try fraction

parse :: Parser a -> String -> Result a
parse x = parseString x mempty

main :: IO ()
main = do
  print $ parse fracOrDec "1/2"
  print $ parse fracOrDec "10.1"
