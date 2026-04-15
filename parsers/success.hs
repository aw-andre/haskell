module Main where

import Text.Trifecta

main :: IO ()
main =
  print $
    parseString
      ( do
          out <- integer
          eof
          return out
      )
      mempty
      "123"
