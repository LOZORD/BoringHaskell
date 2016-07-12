module Fibonacci where

import Data.List (unfoldr)

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

fibs1 :: [Integer]
fibs1 = map (fib) [0..]

-- Exercise 2

-- A helper function for fibs2.
-- Given a pair (a, b), f2' calculates c.
-- From here, unfoldr appends a and uses (b, c) as the value pair for the next
-- iteration of unfoldr on the list.
f2' :: Num n => (n, n) -> Maybe (n, (n, n))
f2' (a, b) =
  let c = a + b
  in Just (a, (b, c))

-- Create a list via unfoldr using the initial pair (0, 1).
fibs2 :: [Integer]
fibs2 = unfoldr (f2') (0, 1)
