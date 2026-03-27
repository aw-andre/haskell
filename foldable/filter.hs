module Main where

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF = foldMap . (\f x -> if f x then pure x else mempty)

main :: IO ()
main = do
  print $ (filterF ((/= 5) :: Int -> Bool) [1, 2, 3, 4, 5, 6, 5, 7] :: [Int])
