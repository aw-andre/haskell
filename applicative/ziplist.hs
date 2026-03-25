module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

extend :: List a -> List a
extend xs = go xs xs
 where
  go :: List a -> List a -> List a
  go Nil orig = go orig orig
  go (Cons x xs) orig = Cons x $ go xs orig

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
  pure = extend . (`Cons` Nil)

  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> Cons x xs = Cons (f x) $ fs <*> xs

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency [(1, return Nil), (2, return $ Cons x xs)]

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance (Eq a) => EqProp (List a) where
  xs =-= ys = take' 100 xs `eq` take' 100 ys

main :: IO ()
main = do
  quickBatch $ applicative $ Cons ("b", "w", (1 :: Int)) Nil

-- print $ pure id <*> (Cons 1 $ Cons 2 $ Cons 3 $ Nil)
