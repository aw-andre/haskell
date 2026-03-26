module Main where
import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ f <$> x

main :: IO ()
main = do
  print $ bind (\x -> [x, "hello"]) ["Andre", "Jakub"]
