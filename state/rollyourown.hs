module Main where

import System.Random (Random (randomR), StdGen)

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN threshold gen = go 0 0 gen
 where
  go :: Int -> Int -> StdGen -> Int
  go tot count gen
    | tot >= threshold = count
    | otherwise = go (tot + die) (count + 1) nextGen
  (die, nextGen) = randomR (1, 6) gen

rollsCountLogged :: Int -> StdGen -> (Int, [Int])
rollsCountLogged threshold gen = go 0 0 gen []
 where
  go :: Int -> Int -> StdGen -> [Int] -> (Int, [Int])
  go tot count gen rolls
    | tot >= threshold = (count, rolls)
    | otherwise = go (tot + die) (count + 1) nextGen (rolls ++ [die])
  (die, nextGen) = randomR (1, 6) gen

main :: IO ()
main = do
  print 1
