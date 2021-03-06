{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where

import ExprT
import Parser (parseExp)
import qualified StackVM as SVM (StackExp(..), Program, stackVM, StackVal)
import qualified Data.Map as M

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

-- Exercise 5

-- this is a little trick that allows us to make and instance of Expr on a
-- Program, which is just a list of StackExp's
newtype ProgramWrapper = ProgramWrapper SVM.Program

instance Expr ProgramWrapper where
  lit n = ProgramWrapper ([SVM.PushI n])
  add (ProgramWrapper p1) (ProgramWrapper p2)
    = ProgramWrapper (p1 ++ p2 ++ [SVM.Add])
  mul (ProgramWrapper p1) (ProgramWrapper p2)
    = ProgramWrapper (p1 ++ p2 ++ [SVM.Mul])

compile :: String -> Maybe SVM.Program
compile input =
  let parse = parseExp lit add mul input :: Maybe ProgramWrapper
  in case parse of
    Nothing -> Nothing
    Just (ProgramWrapper program) -> Just program

compileAndRun :: String -> Either String SVM.StackVal
compileAndRun input =
  let program = compile input
  in case program of
    Nothing -> Left "Could not compile!"
    Just (program) -> SVM.stackVM program

-- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = VELit Integer
           | VEAdd VarExprT VarExprT
           | VEMul VarExprT VarExprT
           | VEVar String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit n = VELit n
  add v1 v2 = VEAdd v1 v2
  mul v1 v2 = VEMul v1 v2

instance HasVars VarExprT where
  var s = VEVar s

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

-- A helper function for the below instance definition
-- There are built-in functions that do the same as this function,
-- but they use a lot of theory jargon (Applicative, Functor, Monad, etc.)
binaryMaybeOp :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
-- operate if we can: (this pattern will always be tested first)
binaryMaybeOp op (Just n1) (Just n2) = Just (op n1 n2)
-- otherwise, don't: (this is the "fall-back" pattern)
binaryMaybeOp _ _ _ = Nothing

-- Although we are implementing an Expr instance on a function, we can treat
-- the Map `map` as just another argument because of currying.
-- I think this helps for readability.
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n map = Just n
  add v1 v2 map = binaryMaybeOp (+) (v1 map) (v2 map)
  mul v1 v2 map = binaryMaybeOp (*) (v1 map) (v2 map)

-- Given key-value pairs and an expression, eval the expression using the map
-- generated by the pairs
withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
