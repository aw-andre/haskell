module Main where

import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbNumber 1
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr consdate []
 where
  consdate :: DatabaseItem -> [UTCTime] -> [UTCTime]
  consdate (DbDate dt) = (:) dt
  consdate _ = id

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr consint []
 where
  consint :: DatabaseItem -> [Integer] -> [Integer]
  consint (DbNumber int) = (:) int
  consint _ = id

mostRecent :: [DatabaseItem] -> Maybe UTCTime
mostRecent = foldr mostRecentDate Nothing
 where
  mostRecentDate :: DatabaseItem -> Maybe UTCTime -> Maybe UTCTime
  mostRecentDate (DbDate dt) = max (Just dt)
  mostRecentDate _ = id

sumDb :: [DatabaseItem] -> Maybe Integer
sumDb = foldr sumInt Nothing
 where
  sumInt :: DatabaseItem -> Maybe Integer -> Maybe Integer
  sumInt (DbNumber int) = maybeAdd (Just int)
  sumInt _ = id

  maybeAdd :: Maybe Integer -> Maybe Integer -> Maybe Integer
  maybeAdd (Just x) (Just y) = Just $ x + y
  maybeAdd (Just x) _ = Just x
  maybeAdd _ (Just x) = Just x
  maybeAdd _ _ = Nothing

avgDb :: [DatabaseItem] -> Maybe Integer
avgDb db = totOverCt total count
 where
  totOverCt Nothing _ = Nothing
  totOverCt _ 0 = Nothing
  totOverCt (Just tot) ct = Just $ div tot ct

  total = sumDb db
  count = toInteger $ length $ filter isDbNum db

  isDbNum (DbNumber _) = True
  isDbNum _ = False

main :: IO ()
main = do
  print $ filterDbDate theDatabase
  print $ filterDbNumber theDatabase
  print $ mostRecent theDatabase
  print $ mostRecent []
  print $ sumDb theDatabase
  print $ sumDb []
  print $ avgDb theDatabase
  print $ avgDb []
