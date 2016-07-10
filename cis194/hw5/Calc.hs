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

-- Exercise 4

instance Expr Integer where
  lit n = n
  add i1 i2 = i1 + i2
  mul i1 i2 = i1 * i2

instance Expr Bool where
  lit n = n > 0
  add b1 b2 = b1 || b2
  mul b1 b2 = b1 && b2

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax m1) (MinMax m2) = MinMax (max m1 m2)
  mul (MinMax m1) (MinMax m2) = MinMax (min m1 m2)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 m1) (Mod7 m2) = Mod7 ((m1 + m2) `mod` 7)
  mul (Mod7 m1) (Mod7 m2) = Mod7 ((m1 * m2) `mod` 7)

-- Testing
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer  -- Just -7
testBool    = testExp :: Maybe Bool     -- Just True
testMM      = testExp :: Maybe MinMax   -- Just (MinMax 5)
testSat     = testExp :: Maybe Mod7     -- Just (Mod7 0)
