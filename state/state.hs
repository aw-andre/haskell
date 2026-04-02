module Main where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State g) = State $ (\(x, gen) -> (f x, gen)) <$> g

instance Applicative (State s) where
  pure x = State $ \y -> (x, y)
  State f <*> State g = State $ (\(h, _) (x, s2) -> (h x, s2)) <$> f <*> g

instance Monad (State s) where
  State f >>= gtostate = State $ (\(x, gen) -> runState (gtostate x) gen) <$> f

get :: State s s
get = State $ \x -> (x, x)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s

eval :: State s a -> s -> a
eval s = fst <$> runState s

modify :: (s -> s) -> State s ()
modify f = State $ \x -> ((), f x)

main :: IO ()
main = do
  print $ runState get "curryIsAmaze"
  print $ runState (put "blah") "woot"
  print $ exec (put "wilma") "daphne"
  print $ exec get "scooby papu"
  print $ eval get "bunnicula"
  print $ eval get "stake a bunny"
  print $ runState (modify (+ 1)) 0
  print $ runState (modify (+ 1) >> modify (+ 1)) 0
