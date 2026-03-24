module Main where

main :: IO ()
main = do
  print $ (fmap (+1) $ read "[1]" :: [Int])
  print $ (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
  print $ fmap (*2) (\x -> x - 2) 1
  print $ fmap ((return '1' ++) . show) (\x -> [x, 1..3]) 0
  -- print $ (*3) $ read $ ("123"++) $ fmap show (readIO "1" :: IO Integer)
  x <- fmap (*3) $ fmap (read :: [Char] -> Integer) $ fmap ("123"++) $ fmap show (readIO "1" :: IO Integer)
  print x
