module Main where

notThe :: String -> Maybe String
notThe x
  | x == "the" = Nothing
  | otherwise = Just x

replaceThe :: String -> String
replaceThe str = unwords $ map replaceThe (words str)
 where
  replaceThe str =
    case notThe str of
      Nothing -> "a"
      Just x -> x

isVowel :: Char -> Bool
isVowel = (`elem` "aeiouAEIOU")

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = countThe False $ words str
 where
  countThe :: Bool -> [String] -> Integer
  countThe _ [] = 0
  countThe _ ([] : _) = 0
  countThe bool (str@(char : _) : strs) = (if bool && isVowel char then 1 else 0) + countThe (notThe str == Nothing) strs

isConsonant :: Char -> Bool
isConsonant = (`elem` "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ")

countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowel

countConsonants :: String -> Integer
countConsonants = toInteger . length . filter isConsonant

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str
  | countVowels str > countConsonants str = Nothing
  | otherwise = Just $ Word' str

main :: IO ()
main = do
  print $ replaceThe "the cow loves us"
  print $ countTheBeforeVowel "the cow"
  print $ countTheBeforeVowel "the ant"
  print $ countVowels "hello"
  print $ countConsonants "hello"
  print $ mkWord "hello"
  print $ mkWord "area"
