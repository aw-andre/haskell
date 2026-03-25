module Main where

import Test.QuickCheck (Arbitrary (arbitrary), elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure x) = Failure x
  fmap f (Success x) = Success $ f x

instance (Monoid e) => Applicative (Validation e) where
  pure = Success
  Failure x <*> Failure y = Failure $ x <> y
  Failure x <*> _ = Failure x
  _ <*> Failure x = Failure x
  Success f <*> Success x = Success $ f x

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Failure x, Success y]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative $ (Success ("b", "w", 1) :: Validation [String] (String, String, Int))
