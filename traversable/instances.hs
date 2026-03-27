{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Main where

import Data.Monoid (Sum)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type Tra a = a (Sum Int, Sum Int, Int, Sum Int)

----------------------------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Ord, Show, Foldable, Functor, Arbitrary, EqProp)

instance Traversable Identity where
  sequenceA (Identity x) = Identity <$> x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = f <$> Identity x

----------------------------------------------------------------------------------------------------
newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show, Foldable, Functor, Arbitrary, EqProp)

instance Traversable (Constant a) where
  sequenceA (Constant x) = pure $ Constant x

----------------------------------------------------------------------------------------------------
data Optional a = Nada | Yep a deriving (Eq, Ord, Show, Foldable, Functor)

instance Traversable Optional where
  sequenceA Nada = pure Nada
  sequenceA (Yep x) = Yep <$> x

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    elements [Nada, Yep x]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

----------------------------------------------------------------------------------------------------
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show, Foldable, Functor)

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return Nil), (2, return $ Cons x y)]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

----------------------------------------------------------------------------------------------------
data Three a b c = Three a b c deriving (Eq, Ord, Show, Foldable, Functor)

instance Traversable (Three a b) where
  sequenceA (Three x y z) = Three x y <$> z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    (x, y, z) <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

----------------------------------------------------------------------------------------------------
data Four a b = Four a b b b deriving (Eq, Ord, Show, Foldable, Functor)

instance Traversable (Four a) where
  sequenceA (Four w x y z) = Four w <$> x <*> y <*> z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four a b) where
  arbitrary = do
    (w, x, y, z) <- arbitrary
    return $ Four w x y z

instance (Eq a, Eq b) => EqProp (Four a b) where
  (=-=) = eq

----------------------------------------------------------------------------------------------------
data S n a = S (n a) a deriving (Eq, Show, Foldable, Functor)

instance (Traversable n) => Traversable (S n) where
  sequenceA (S nfx fx) = S <$> sequenceA nfx <*> fx

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

----------------------------------------------------------------------------------------------------
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show, Foldable, Functor)

instance Traversable Tree where
  sequenceA Empty = pure Empty
  sequenceA (Leaf fx) = Leaf <$> fx
  sequenceA (Node tx fy tz) = Node <$> sequenceA tx <*> fy <*> sequenceA tz

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    (x, y, z) <- arbitrary
    elements [Empty, Leaf y, Node x y z]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  quickBatch $ traversable $ (undefined :: Tra Identity)
  quickBatch $ traversable $ (undefined :: Tra (Constant Int))
  quickBatch $ traversable $ (undefined :: Tra List)
  quickBatch $ traversable $ (undefined :: Tra (Three Int Int))
  quickBatch $ traversable $ (undefined :: Tra (S []))
  quickBatch $ traversable $ (undefined :: Tra Tree)
