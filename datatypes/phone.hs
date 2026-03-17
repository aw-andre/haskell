module Main where

import Data.Char
import Data.Foldable (find)
import Data.Maybe (fromJust)

data Combination = Combination {int :: Int, char :: Char} deriving (Show)
data PhoneChar = PhoneChar Char Combination deriving (Show)

data Phone = Phone [PhoneChar] deriving (Show)

phone :: Phone
phone =
  Phone
    [ PhoneChar '1' $ Combination 1 '1'
    , PhoneChar 'a' $ Combination 1 '2'
    , PhoneChar 'b' $ Combination 2 '2'
    , PhoneChar 'c' $ Combination 3 '2'
    , PhoneChar '2' $ Combination 4 '2'
    , PhoneChar 'd' $ Combination 1 '3'
    , PhoneChar 'e' $ Combination 2 '3'
    , PhoneChar 'f' $ Combination 3 '3'
    , PhoneChar '3' $ Combination 4 '3'
    , PhoneChar 'g' $ Combination 1 '4'
    , PhoneChar 'h' $ Combination 2 '4'
    , PhoneChar 'i' $ Combination 3 '4'
    , PhoneChar '4' $ Combination 4 '4'
    , PhoneChar 'j' $ Combination 1 '5'
    , PhoneChar 'k' $ Combination 2 '5'
    , PhoneChar 'l' $ Combination 3 '5'
    , PhoneChar '5' $ Combination 4 '5'
    , PhoneChar 'm' $ Combination 1 '6'
    , PhoneChar 'n' $ Combination 2 '6'
    , PhoneChar 'o' $ Combination 3 '6'
    , PhoneChar '6' $ Combination 4 '6'
    , PhoneChar 'p' $ Combination 1 '7'
    , PhoneChar 'q' $ Combination 2 '7'
    , PhoneChar 'r' $ Combination 3 '7'
    , PhoneChar 's' $ Combination 4 '7'
    , PhoneChar '7' $ Combination 5 '7'
    , PhoneChar 't' $ Combination 1 '8'
    , PhoneChar 'u' $ Combination 2 '8'
    , PhoneChar 'v' $ Combination 3 '8'
    , PhoneChar '8' $ Combination 4 '8'
    , PhoneChar 'w' $ Combination 1 '9'
    , PhoneChar 'x' $ Combination 2 '9'
    , PhoneChar 'y' $ Combination 3 '9'
    , PhoneChar 'z' $ Combination 4 '9'
    , PhoneChar '9' $ Combination 5 '9'
    , PhoneChar '0' $ Combination 1 '0'
    , PhoneChar '+' $ Combination 2 '0'
    , PhoneChar '_' $ Combination 3 '0'
    , PhoneChar ' ' $ Combination 4 '0'
    , PhoneChar '^' $ Combination 1 '*'
    , PhoneChar '*' $ Combination 2 '*'
    , PhoneChar '.' $ Combination 1 '#'
    , PhoneChar ',' $ Combination 2 '#'
    , PhoneChar '#' $ Combination 3 '#'
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

normalize :: String -> [[Char]]
normalize [] = []
normalize (x : xs) =
  if isUpper x
    then ['*', toLower x] : normalize xs
    else [x] : normalize xs

getKey :: Phone -> Char -> Maybe Combination
getKey (Phone phonechars) char =
  case find (\(PhoneChar c _) -> c == char) phonechars of
    Just (PhoneChar _ comb) -> Just comb
    Nothing -> Nothing

foldMaybe :: [Maybe a] -> Maybe [a]
foldMaybe = foldr maybeCons $ Just []
 where
  maybeCons :: Maybe a -> Maybe [a] -> Maybe [a]
  maybeCons (Just x) (Just xs) = Just $ x : xs
  maybeCons _ _ = Nothing

reverseTaps :: Phone -> String -> Maybe [Combination]
reverseTaps phone str = foldMaybe $ helper phone $ normalize str
 where
  helper :: Phone -> [[Char]] -> [Maybe Combination]
  helper phone chars = concat $ map (map $ getKey phone) chars

reverseConvo :: Phone -> [String] -> Maybe [Combination]
reverseConvo phone strs =
  case foldMaybe $ map (reverseTaps phone) strs of
    Just x -> Just $ concat x
    Nothing -> Nothing

fingerTaps :: [Combination] -> Int
fingerTaps = sum . map (\(Combination n _) -> n)

letterCost :: Phone -> String -> Char -> Maybe Int
letterCost phone str char =
  case maybeCombs phone str char of
    Nothing -> Nothing
    Just combs -> Just $ sum $ map int combs
 where
  maybeCombs phone str char =
    reverseTaps
      phone
      $ concat
      $ filter (\chars -> find (\c -> c == char) chars /= Nothing)
      $ normalize str

keyPairs :: (Eq a) => [a] -> [(Int, a)] -> [(Int, a)]
keyPairs [] pairs = pairs
keyPairs (x : xs) pairs = keyPairs xs $ addOne x pairs
 where
  addOne :: (Eq a) => a -> [(Int, a)] -> [(Int, a)]
  addOne x [] = [(1, x)]
  addOne x ((n, x') : xs) = (if x' == x then n + 1 else n, x') : addOne x xs

mostPopularLetter :: String -> Maybe Char
mostPopularLetter [] = Nothing
mostPopularLetter str = Just $ snd $ maximum $ keyPairs (concat $ normalize str) []

coolestLtr :: [String] -> Maybe Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> Maybe String
coolestWord [] = Nothing
coolestWord strs = Just $ snd $ maximum $ keyPairs strs []

main :: IO ()
main = do
  print $ reverseConvo phone convo
  print $ fingerTaps $ fromJust $ reverseConvo phone convo
  print $ letterCost phone (concat convo) 'w'
  print $ mostPopularLetter $ concat convo
  print $ coolestLtr convo
  print $ coolestWord convo
