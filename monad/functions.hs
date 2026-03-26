module Main where

import Control.Monad (join)

j :: (Monad m) => m (m a) -> m a
j = join

l1 :: (Monad m) => (a -> b) -> m a -> m b
l1 = fmap

l2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

a :: (Monad m) => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: (Monad m) => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) f = f x >>= (`cons'` meh xs f)
 where
  cons' :: (Monad m) => a -> m [a] -> m [a]
  cons' x mxs = mxs >>= (return . (x :))

flipType :: (Monad m) => [m a] -> m [a]
flipType = (`meh` (>>= return))

main :: IO ()
main = do
  print $ j [[1, 2], [], [3]]
  print $ j $ Just $ Just 1
  print $ j $ Just (Nothing :: Maybe Int)
  print $ j (Nothing :: Maybe (Maybe Int))
