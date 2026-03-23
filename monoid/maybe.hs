module Main where

import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (Testable (property), Gen, frequency, sample, Arbitrary (arbitrary))

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance (Monoid a) => Monoid (Optional a) where
  mempty = Only (mempty :: a)

instance (Semigroup a) => Semigroup (Optional a) where
  Nada <> Nada = Nada
  (Only x) <> Nada = Only x
  Nada <> (Only x) = Only x
  (Only x) <> (Only y) = Only $ x <> y

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  x@(First' (Only _)) <> _ = x
  _ <> x@(First' (Only _)) = x
  x@_ <> _ = x

instance Monoid (First' a) where
  mempty = First' $ Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return $ First' Nada), (9, return $ First' $ Only x)]

type F = First' Int

main :: IO ()
main = do
  hspec $ do
    it "assoc" $ property $ \x y z -> ((x :: F) <> (y :: F)) <> (z :: F) == x <> (y <> z)
    describe "mempty" $ do
      it "left identity" $ property $ \x -> (mempty :: F) <> (x :: F) == x
      it "right identity" $ property $ \x -> (x :: F) <> (mempty :: F) == x
