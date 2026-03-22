module Main (main) where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (findIndices, intersperse)
import Data.Maybe (fromJust, isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

data WordList = WordList [String]

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 7

gameWords :: IO WordList
gameWords = do
  allWords <- readFile "data/words.txt"
  return $
    WordList $
      filter (\x -> length x >= minWordLength && length x <= maxWordLength) $
        lines allWords

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  index <- randomRIO (0, length wl - 1)
  return $ wl !! index

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
      ++ " Guessed so far: "
      ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (take (length str) (repeat Nothing)) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) = (`elem` word)

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) = (`elem` guessed)

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter puzzle@(Puzzle word inc guessed) c
  | alreadyGuessed puzzle c = puzzle
  | not $ charInWord puzzle c =
      Puzzle
        word
        inc
        (c : guessed)
  | otherwise =
      Puzzle
        word
        (zipWith (\x y -> if x == c then Just c else y) word inc)
        (c : guessed)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (False, _) -> do
      putStrLn "Sorry, that character isn't in the word."
      return $ fillInCharacter puzzle guess
    (True, _) -> do
      putStrLn "Great! That character is in the word!"
      return $ fillInCharacter puzzle guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word inc guessed)
  | wrongGuesses inc guessed > 7 =
      do
        putStrLn "You lose!"
        putStrLn $ "The word was: " ++ word
        exitSuccess
  | otherwise = return ()
 where
  wrongGuesses :: [Maybe Char] -> [Char] -> Int
  wrongGuesses _ [] = 0
  wrongGuesses inc (guess : guesses) =
    if elem guess $ map fromJust $ filter isJust inc then 0 else 1 + wrongGuesses inc guesses

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _)
  | all isJust filledInSoFar =
      do
        putStrLn "You win!"
        exitSuccess
  | otherwise = return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- gameWords >>= randomWord
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
