module Main where

import Test.QuickCheck (Arbitrary (arbitrary), Fun (Fun), frequency, quickCheck)

fc ::
  (Eq (f a), Eq (f c), Functor f) =>
  f a ->
  Fun a b ->
  Fun b c ->
  Bool
fc x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x) && fmap id x == x

type FC a = a Int -> Fun Int Int -> Fun Int Int -> Bool

----------------------------------------------------------------------------------------------------
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers $ f x

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return LolNope), (9, return $ Yeppers x)]

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  quickCheck $ (fc :: FC Possibly)
