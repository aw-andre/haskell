{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Char (toUpper)
import Data.List (sort, intersperse)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (Testable (property), Gen, frequency, sample)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "Invalid digit"

digits :: Int -> [Int]
digits 0 = [0]
digits n = reverse $ helper n
 where
  helper :: Int -> [Int]
  helper 0 = []
  helper n = (n `mod` 10) : helper (n `div` 10)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
 where
  go _ status@(_, False) = status
  go y (Nothing, t) = (Just y, t)
  go y (Just x, t) = (Just y, x >= y)

isAssociative :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
isAssociative op x y z = x `op` (y `op` z) == (x `op` y) `op` z

isCommutative :: (Eq a) => (a -> a -> a) -> a -> a -> Bool
isCommutative op x y = x `op` y == y `op` x

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative = isAssociative (+)

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative = isCommutative (+)

timesAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
timesAssociative = isAssociative (*)

timesCommutative :: (Eq a, Num a) => a -> a -> Bool
timesCommutative = isCommutative (*)

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs) = toUpper x : xs

f :: String -> Bool
f x = (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

f' :: (Ord a) => [a] -> Bool
f' x = (sort x == twice sort x) && (sort x == fourTimes sort x)

instance (Show a, Show b) => Show (a -> b) where
  show _ = "<function>"

data Fool = Fulse | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = frequency [(2, return Fulse), (1, return Frue)]

main :: IO ()
main = do
  sample foolGen
  hspec $ do
    describe "digitToWord" $ do
      it "returns zero for 0" $ do
        digitToWord 0 `shouldBe` "zero"
      it "returns one for 1" $ do
        digitToWord 1 `shouldBe` "one"
    describe "digits" $ do
      it "returns [1] for 1" $ do
        digits 1 `shouldBe` [1]
      it "returns [1, 0, 0] for 100" $ do
        digits 100 `shouldBe` [1, 0, 0]
    describe "wordNumber" $ do
      it "one-zero-zero given 100" $ do
        wordNumber 100 `shouldBe` "one-zero-zero"
      it "nine-zero-zero-one for 9001" $ do
        wordNumber 9001 `shouldBe` "nine-zero-zero-one"
    it "half" $ do
      property $ \x -> ((* 2) . (/ 2)) x == (x :: Rational)
    it "sort" $ do
      property $ \x -> listOrdered $ sort (x :: [Int])
    describe "plus" $ do
      it "associative" $ do
        property $ \x y z -> plusAssociative (x :: Int) (y :: Int) (z :: Int)
      it "commutative" $ do
        property $ \x y -> plusCommutative (x :: Int) (y :: Int)
    describe "times" $ do
      it "associative" $ do
        property $ \x y z -> timesAssociative (x :: Int) (y :: Int) (z :: Int)
      it "commutative" $ do
        property $ \x y -> timesCommutative (x :: Int) (y :: Int)
    describe "quotRem" $ do
      it "quot" $ do
        property $ \x y -> (quot (x :: Int) (y :: Int)) * y + (rem x y) == x
      it "div" $ do
        property $ \x y -> (div (x :: Int) (y :: Int)) * y + (mod x y) == x
    it "reverse" $ do
      property $ \x -> (reverse . reverse) (x :: [Int]) == id x
    describe "func" $ do
      it "$" $ do
        property $ \f x -> ((f :: Int -> Int) $ (x :: Int)) == f x
      it "." $ do
        property $ \f g x -> ((f :: [Int] -> Int) . (g :: Int -> [Int])) (x :: Int) == f (g x)
    describe "list" $ do
      it ":" $ do
        property $ \x start -> foldr (:) (start :: [Int]) (x :: [Int]) == x ++ start
      it "++" $ do
        property $ \x -> foldr (++) [] (x :: [[Int]]) == concat x
    it "take" $ do
      property $ \n xs -> length (take n (xs :: [Int])) == n
    it "read" $ do
      property $ \x -> read (show (x :: Int)) == x
    it "square" $ do
      property $ \x -> (x :: Float) == sqrt x * sqrt x
    describe "idempotence" $ do
      it "capitalizeWord" $ do
        property $ \x -> f x
      it "sort" $ do
        property $ \x -> f' (x :: [Int])
