module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> x = x
  x <> Nil = x
  Cons x xs <> ys = Cons x $ xs <> ys

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure = (`Cons` Nil)

  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency [(1, return Nil), (2, return $ Cons x xs)]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative $ Cons ("b", "w", (1 :: Int)) Nil
