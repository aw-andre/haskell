module Main where

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

main :: IO ()
main = do
  print $ fmap (+ 1) $ Identity 1
  print $ Identity (+ 1) <*> Identity (1)
