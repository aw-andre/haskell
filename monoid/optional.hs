module Main where

import Data.Monoid

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance (Monoid a) => Monoid (Optional a) where
  mempty = Only (mempty :: a)

instance (Semigroup a) => Semigroup (Optional a) where
  Nada <> Nada = Nada
  (Only x) <> Nada = Only x
  Nada <> (Only x) = Only x
  (Only x) <> (Only y) = Only $ x <> y

main :: IO ()
main = do
  print $ mappend (Only (Sum (1 :: Int))) (Only (Sum (1 :: Int)))
