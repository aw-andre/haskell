module Main where

import Test.QuickCheck (Arbitrary (arbitrary), Fun (Fun), quickCheck, elements)

fc ::
  (Eq (f a), Eq (f c), Functor f) =>
  f a ->
  Fun a b ->
  Fun b c ->
  Bool
fc x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x) && fmap id x == x

type FC a = a Int -> Fun Int Int -> Fun Int Int -> Bool

----------------------------------------------------------------------------------------------------
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second $ f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [First x, Second y]

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  quickCheck $ (fc :: FC (Sum Int))
