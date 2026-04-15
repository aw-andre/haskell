module Main where

import Text.Parser.Combinators
import Text.Trifecta

stop :: Parser Char
stop = unexpected "stop"

testAllParserStrings strs psrs =
  (\str psr -> parseString psr mempty str) <$> strs <*> psrs

-- example :: String -> Parser Char
example str = foldr (>>) (return ()) $ fmap (\x -> char x) str

main :: IO ()
main = print $ testAllParserStrings ["123"] $ fmap example ["1", "12", "123"]
