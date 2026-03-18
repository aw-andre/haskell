module Main where

import Data.Maybe (fromJust)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee fallback _ Nothing = fallback
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe = (`mayybee` id)


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (: [])

catMaybes :: [Maybe a] -> [a]
catMaybes = map fromJust . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr maybeCons $ Just []
 where
  maybeCons :: Maybe a -> Maybe [a] -> Maybe [a]
  maybeCons (Just x) (Just xs) = Just $ x : xs
  maybeCons _ _ = Nothing

main :: IO ()
main = do
  print $ isJust $ Just 1
  print $ isJust $ Nothing
  print $ isNothing $ Just 1
  print $ isNothing $ Nothing
  print $ mayybee 0 (+1) Nothing
  print $ mayybee 0 (+1) (Just 1)
  print $ fromMaybe 0 Nothing
  print $ fromMaybe 0 (Just 1)
  print $ listToMaybe [1, 2, 3]
  print $ listToMaybe ([] :: [Int])
  print $ maybeToList (Just 1)
  print $ maybeToList (Nothing :: Maybe Int)
  print $ catMaybes [Just 1, Nothing, Just 2]
  print $ catMaybes $ take 3 $ repeat (Nothing :: Maybe Int)
  print $ flipMaybe [Just 1, Just 2, Just 3]
  print $ flipMaybe [Just 1, Nothing, Just 3]
