module Calc where

import ExprT
import Parser (parseExp)

-- Exercise 1

eval :: ExprT -> Integer
-- base case
eval (Lit n) = n
-- recursive cases
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr input =
  let parse = parseExp (Lit) (Add) (Mul) input
  in case parse of
    Nothing -> Nothing
    Just expr -> Just (eval expr)

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit n = (Lit n)
  add e1 e2 = (Add e1 e2)
  mul e1 e2 = (Mul e1 e2)

reify :: ExprT -> ExprT
reify = id
