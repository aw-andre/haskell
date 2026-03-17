module Main where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add n1 n2) = (+) (eval n1) (eval n2)

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add n1 n2) = printExpr n1 ++ "+" ++ printExpr n2

expr :: Expr
expr = Add (Add (Lit 10) (Lit 20)) (Add (Lit 30) (Lit 40))

main :: IO ()
main = do
  print $ eval expr
  print $ printExpr expr
