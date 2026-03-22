module Main where
import System.Exit (exitFailure)

type Name = String
type Age = Integer
data Person = Person Name Age deriving (Show)
data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $
        PersonInvalidUnknown $
          "Name was: "
            ++ show name
            ++ " Age was: "
            ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Age: "
  age <- getLine
  putStr "Name: "
  name <- getLine
  case mkPerson name (read age) of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      exitFailure
    Right person -> do
      putStrLn $ "Yay! Successfully got a person: " ++ show person

main :: IO ()
main = do
  gimmePerson
