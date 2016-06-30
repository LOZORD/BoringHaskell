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

-- Exercise 3 -- TODO
