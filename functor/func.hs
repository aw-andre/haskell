module Main where

import Test.QuickCheck (Arbitrary (arbitrary), Fun (Fun), quickCheck)

fc ::
  (Eq (f a), Eq (f c), Functor f) =>
  f a ->
  Fun a b ->
  Fun b c ->
  Bool
fc x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x) && fmap id x == x

type FC a = a Int -> Fun Int Int -> Fun Int Int -> Bool

----------------------------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Show, Arbitrary)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

----------------------------------------------------------------------------------------------------
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

----------------------------------------------------------------------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

----------------------------------------------------------------------------------------------------
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y $ f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    (w, x, y, z) <- arbitrary
    return $ Four' w x y z

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  quickCheck $ (fc :: FC Identity)
  quickCheck $ (fc :: FC Pair)
  quickCheck $ (fc :: FC (Two Int))
  quickCheck $ (fc :: FC (Four' Int))
