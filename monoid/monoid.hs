module Main where

import Data.Monoid (Sum)
import Test.Hspec
import Test.QuickCheck

monoidTest :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidTest x y z =
  (x <> (y <> z)) == ((x <> y) <> z) && mempty <> x == x && x <> mempty == x

data Mon a = Mon {getFunc :: (a -> a -> a -> Bool)}

type SumInt = Sum Int

----------------------------------------------------------------------------------------------------
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

----------------------------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Show, Arbitrary)

instance (Semigroup a) => Semigroup (Identity a) where
  Identity x <> Identity y = Identity $ x <> y

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

----------------------------------------------------------------------------------------------------
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four w1 x1 y1 z1 <> Four w2 x2 y2 z2 =
    Four (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty

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

instance Monoid BoolConj where
  mempty = BoolConj True

----------------------------------------------------------------------------------------------------
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show, Arbitrary)

instance Semigroup BoolDisj where
  BoolDisj True <> _ = BoolDisj True
  _ <> BoolDisj True = BoolDisj True
  _ <> _ = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False

----------------------------------------------------------------------------------------------------
instance Show (a -> b) where
  show _ = "<function>"

newtype Combine a b = Combine {unCombine :: (a -> b)} deriving (Show)

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \x -> f x <> g x

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    x <- arbitrary
    return $ Combine x

----------------------------------------------------------------------------------------------------
newtype Comp a = Comp {unComp :: (a -> a)} deriving (Show)

instance (Semigroup a) => Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ \x -> f x <> g x

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    x <- arbitrary
    return $ Comp x

----------------------------------------------------------------------------------------------------
newtype Mem s a = Mem {runMem :: s -> (a, s)} deriving (Show)

instance (Semigroup a) => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ \x -> (fst (f x) <> fst (g x), snd (g (snd (f x))))

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty, x)

instance (Arbitrary s, CoArbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = do
    x <- arbitrary
    return $ Mem x

----------------------------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
  it "trivial" $ property $ getFunc (Mon monoidTest :: Mon Trivial)
  it "identity" $ property $ getFunc (Mon monoidTest :: Mon (Identity SumInt))
  it "four" $ property $ getFunc (Mon monoidTest :: Mon (Four SumInt SumInt SumInt SumInt))
  it "boolconj" $ property $ getFunc (Mon monoidTest :: Mon BoolConj)
  it "booldisj" $ property $ getFunc (Mon monoidTest :: Mon BoolDisj)
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
  it "mem" $
    property $
      \f g h x ->
        runMem
          (((f :: Mem SumInt String) <> (g :: Mem SumInt String)) <> (h :: Mem SumInt String))
          (x :: SumInt)
          == runMem (f <> (g <> h)) x
