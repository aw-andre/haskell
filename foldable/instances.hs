module Main where

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant x) = f x

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two x y) = f y

data Four a b = Four a b b b

instance Foldable (Four a) where
  foldMap f (Four w x y z) = f x <> f y <> f z

main :: IO ()
main = do
  print 1
