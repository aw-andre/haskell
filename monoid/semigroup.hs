module Main where

import Data.Monoid (Sum)
import Test.Hspec
import Test.QuickCheck

assocTest :: (Eq m, Semigroup m) => m -> m -> m -> Bool
assocTest x y z =
  (x <> (y <> z)) == ((x <> y) <> z)

data Assoc a = Assoc {Mon :: (a -> a -> a -> Bool)}

type SumInt = Sum Int

----------------------------------------------------------------------------------------------------
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

----------------------------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Show, Arbitrary)

instance Semigroup (Identity a) where
  Identity x <> Identity _ = Identity x

----------------------------------------------------------------------------------------------------
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four w1 x1 y1 z1 <> Four w2 x2 y2 z2 =
    Four (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    (w, x, y, z) <- arbitrary
    return $ Four w x y z

----------------------------------------------------------------------------------------------------
newtype BoolConj = BoolConj Bool deriving (Eq, Show, Arbitrary)

instance Semigroup BoolConj where
  BoolConj False <> _ = BoolConj False
  _ <> BoolConj False = BoolConj False
  _ <> _ = BoolConj True

----------------------------------------------------------------------------------------------------
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show, Arbitrary)

instance Semigroup BoolDisj where
  BoolDisj True <> _ = BoolDisj True
  _ <> BoolDisj True = BoolDisj True
  _ <> _ = BoolDisj False

----------------------------------------------------------------------------------------------------
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst x <> Fst y = Fst y
  Snd x <> _ = Snd x
  _ <> Snd x = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Fst x, Snd y]

----------------------------------------------------------------------------------------------------
instance Show (a -> b) where
  show _ = "<function>"

newtype Combine a b = Combine {unCombine :: (a -> b)} deriving (Show)

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \x -> f x <> g x

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    x <- arbitrary
    return $ Combine x

----------------------------------------------------------------------------------------------------
newtype Comp a = Comp {unComp :: (a -> a)} deriving (Show)

instance (Semigroup a) => Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ \x -> f x <> g x

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    x <- arbitrary
    return $ Comp x

----------------------------------------------------------------------------------------------------
data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) where
  Failure' x <> Failure' y = Failure' $ x <> y
  Success' x <> _ = Success' x
  _ <> Success' x = Success' x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Failure' x, Success' y]

----------------------------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
  it "trivial" $ property $ getFunc (Assoc assocTest :: Assoc Trivial)
  it "identity" $ property $ getFunc (Assoc assocTest :: Assoc (Identity Int))
  it "four" $ property $ getFunc (Assoc assocTest :: Assoc (Four SumInt SumInt SumInt SumInt))
  it "boolconj" $ property $ getFunc (Assoc assocTest :: Assoc BoolConj)
  it "booldisj" $ property $ getFunc (Assoc assocTest :: Assoc BoolDisj)
  it "or" $ property $ getFunc (Assoc assocTest :: Assoc (Or Int Int))
  it "combine" $
    property $
      \f g h x ->
        unCombine
          (((f :: Combine Int SumInt) <> (g :: Combine Int SumInt)) <> (h :: Combine Int SumInt))
          (x :: Int)
          == unCombine (f <> (g <> h)) x
  it "comp" $
    property $
      \f g h x ->
        unComp
          (((f :: Comp SumInt) <> (g :: Comp SumInt)) <> (h :: Comp SumInt))
          (x :: SumInt)
          == unComp (f <> (g <> h)) x
  it "validation" $ property $ getFunc (Assoc assocTest :: Assoc (Validation String Int))
