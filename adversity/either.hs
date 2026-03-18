module Main where
import Data.Maybe (isNothing)

lefts' :: [Either a b] -> [a]
-- lefts' = map fromLeft . filter isLeft
lefts' = foldr consIfLeft []
 where
  consIfLeft :: Either a b -> [a] -> [a]
  consIfLeft (Left x) = (x :)
  consIfLeft (Right _) = id

rights' :: [Either a b] -> [b]
-- rights' = map fromRight . filter isRight
rights' = foldr consIfRight []
 where
  consIfRight :: Either a b -> [b] -> [b]
  consIfRight (Right x) = (x :)
  consIfRight (Left _) = id

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' lst = (lefts' lst, rights' lst)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right y) = Just $ f y

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right y) = g y

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' = either' (\_ -> Nothing) . (Just .)

main :: IO ()
main = do
  print $ lefts' [Left 1, Right True, Left 2]
  print $ rights' [Left 1, Right True, Left 2]
  print $ partitionEithers' [Left 1, Right True, Left 2]
  print $ eitherMaybe' even $ Left $ Just 1
  print $ eitherMaybe' even $ Right $ 1
  print $ either' even isNothing $ Left 1
  print $ either' even isNothing $ Right $ Just 1
  print $ eitherMaybe'' even $ Left $ Just 1
  print $ eitherMaybe'' even $ Right $ 1
