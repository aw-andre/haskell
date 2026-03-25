module Main where

import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

----------------------------------------------------------------------------------------------------
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure f = Pair f f
  Pair f g <*> Pair x y = Pair (f x) (g y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

----------------------------------------------------------------------------------------------------
data Four a b c = Four a a b c deriving (Eq, Show)

instance Functor (Four a b) where
  fmap f (Four x x' y z) = Four x x' y $ f z

instance (Monoid a, Monoid b) => Applicative (Four a b) where
  pure = Four mempty mempty mempty
  Four x1 x1' y1 f <*> Four x2 x2' y2 z = Four (x1 <> x2) (x1' <> x2') (y1 <> y2) (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Four a b c) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

instance (Eq a, Eq b, Eq c) => EqProp (Four a b c) where
  (=-=) = eq

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  quickBatch $ applicative $ Pair ("a", "a", "a") ("a", "a", "a")
  quickBatch $ applicative $ Four ("a", "a", "a") ("a", "a", "a") ("a", "a", "a") ("a", "a", "a")
