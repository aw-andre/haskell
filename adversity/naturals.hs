module Main where
import Data.Maybe (fromJust)

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat x
  | x < 0 = Nothing
  | x > 0 = Just $ Succ $ fromJust $ integerToNat $ x - 1
integerToNat _ = error "Unreachable!"

main :: IO ()
main = do
  print $ natToInteger $ Succ $ Succ Zero
  print $ integerToNat $ -2
  print $ integerToNat 0
  print $ integerToNat 2
