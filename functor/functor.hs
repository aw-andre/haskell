{-# LANGUAGE FlexibleInstances #-}

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
data Quant a b = Finance | Desk a | Floor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x
  fmap f (Floor x) = Floor $ f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return Finance), (1, return $ Desk x), (8, return $ Floor y)]

----------------------------------------------------------------------------------------------------
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = do
    x <- arbitrary
    return $ K x

----------------------------------------------------------------------------------------------------
newtype Flip f a b = Flip (f b a) deriving (Eq, Show, Arbitrary)

instance Functor (Flip K b) where
  fmap f (Flip (K x)) = Flip $ K $ f x

----------------------------------------------------------------------------------------------------
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    x <- arbitrary
    return $ GoatyConst x

----------------------------------------------------------------------------------------------------
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut $ fmap f x

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
  arbitrary = do
    x <- arbitrary
    return $ LiftItOut $ x

----------------------------------------------------------------------------------------------------
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ DaWrappa x y

----------------------------------------------------------------------------------------------------
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x $ fmap f y

instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ IgnoringSomething x y

----------------------------------------------------------------------------------------------------
data Notorious f a b c = Notorious (f a) (f b) (f c) deriving (Eq, Show)

instance (Functor f) => Functor (Notorious f a b) where
  fmap f (Notorious x y z) = Notorious x y $ fmap f z

instance (Arbitrary (f a), Arbitrary (f b), Arbitrary (f c)) => Arbitrary (Notorious f a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Notorious x y z

----------------------------------------------------------------------------------------------------
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency [(1, return Nil), (9, return $ Cons x xs)]

----------------------------------------------------------------------------------------------------
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    frequency [(4, return NoGoat), (5, return $ OneGoat w), (1, return $ MoreGoats x y z)]

----------------------------------------------------------------------------------------------------
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str x) = Print str $ f x
  fmap f (Read g) = Read $ fmap f g

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  quickCheck $ (fc :: FC (Quant Int))
  quickCheck $ (fc :: FC (K Int))
  quickCheck $ (fc :: FC (Flip K Int))
  quickCheck $ (fc :: FC (EvilGoateeConst Int))
  quickCheck $ (fc :: FC (LiftItOut []))
  quickCheck $ (fc :: FC (Parappa [] Maybe))
  quickCheck $ (fc :: FC (Notorious [] Int Char))
  quickCheck $ (fc :: FC (List))
  quickCheck $ (fc :: FC (GoatLord))
