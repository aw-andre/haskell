module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type Mon a = a (String, String, Int)
type SSI = (String, String, Int)

----------------------------------------------------------------------------------------------------
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

----------------------------------------------------------------------------------------------------
data Either' a b = Left' a | Right' b deriving (Eq, Show)

instance Functor (Either' a) where
  fmap _ (Left' x) = Left' x
  fmap f (Right' x) = Right' $ f x

instance Applicative (Either' a) where
  pure = Right'
  Left' x <*> _ = Left' x
  _ <*> Left' x = Left' x
  Right' f <*> Right' x = f <$> Right' x

instance Monad (Either' a) where
  Left' x >>= _ = Left' x
  Right' x >>= f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Left' x, Right' y]

instance (Eq a, Eq b) => EqProp (Either' a b) where
  (=-=) = eq

----------------------------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = f <$> Identity x

instance Monad Identity where
  Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

----------------------------------------------------------------------------------------------------
data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil xs = xs
append xs Nil = xs
append (Cons x xs) ys = Cons x $ append xs ys

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

instance Monad List where
  Nil >>= _ = Nil
  Cons x xs >>= f = append (f x) (xs >>= f)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency [(1, return Nil), (2, return $ Cons x xs)]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  quickBatch $ monad $ (NopeDotJpg :: Mon Nope)
  quickBatch $ monad $ (Left' ("", "", 1) :: Either' SSI SSI)
  quickBatch $ monad $ (Identity ("", "", 1) :: Identity SSI)
  quickBatch $ monad $ (Nil :: List SSI)
