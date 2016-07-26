{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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

-- Exercise 3

-- Let's implement Stream like an infinite linked list!
newtype Stream a = Stream (a, Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream (val, next)) = val:(streamToList next)

showN :: Show a => Integer -> Stream a -> String
showN n stream = show (take (fromIntegral n) (streamToList stream))

showStreamAmount = 20

instance Show a => Show (Stream a) where
  show stream = showN showStreamAmount stream

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat val = Stream(val, streamRepeat val)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream (val, next)) = Stream (f val, streamMap f next)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f val =  Stream (val, streamFromSeed f (f val))

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

--  https://www.reddit.com/r/haskellquestions/comments/2qv1nr/help_with_cis194_hw6_question_5/
--  NOTE: the pattern matching for the arguments can cause problems. Since our
--  streams are infinite, eagerly pattern matching on the second stream can
--  cause an "infinite-loop" problem. See the Reddit post for more information.
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream (v1, s1)) s2 = Stream (v1, interleaveStreams s2 s1)

-- https://oeis.org/A001511 (see Burgess, as others have noted)
genRuler :: Integer -> Stream Integer
genRuler n = interleaveStreams (streamRepeat n) (genRuler (n + 1))

ruler :: Stream Integer
ruler = genRuler 0

-- Exercise 6

x :: Stream Integer
-- x = 0 + 1x + 0(x2 + x3 + x4 + ...)
x = Stream (0, Stream (1, streamRepeat 0))

-- for combining two streams element-wise
streamZip :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZip f (Stream (v1, s1)) (Stream (v2, s2)) =
  Stream (f v1 v2, streamZip f s1 s2)

instance Num (Stream Integer) where
  fromInteger n = Stream (n, streamRepeat 0)
  negate stream = streamMap negate stream
  (+) sA sB = streamZip (+) sA sB
  (*) sA sB =
    let Stream (a0, sA') = sA
        Stream (b0, sB') = sB
        a0b0 = a0 * b0
        a0sB' = streamMap (a0 *) sB'
        sA'sB = sA' * sB
    in Stream (a0b0, a0sB' + sA'sB)

instance Fractional (Stream Integer) where
  (/) sA sB =
    let Stream (a0, sA') = sA
        Stream (b0, sB') = sB
        -- use `div` instead of `/` to preserve Integer type
        sQ = Stream (div a0 b0, streamMap (\ n -> div n b0) (sA' - (sQ * sB')))
    in sQ

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7

-- a 2x2 matrix of integers
-- Matrix (a, b, c, d) <=> [a b ; c d]
newtype Matrix = Matrix (Integer, Integer, Integer, Integer) deriving (Show, Eq)

scalarMult n (Matrix (a, b, c, d)) =
  Matrix ((n * a), (n * b), (n * c), (n * d))

instance Num Matrix where
  -- we really only care about Matrix multiplication
  (*) (Matrix (a, b, c, d)) (Matrix (e, f, g, h)) =
    let i = a * e + b * g
        j = a * f + b * h
        k = c * e + d * g
        l = c * f + d * h
    in Matrix (i, j, k, l)
  -- however, I added these "for fun"
  fromInteger n = Matrix (n, n, n, n)
  negate matrix = scalarMult (-1) matrix
  (+) (Matrix (a, b, c, d)) (Matrix (e, f, g, h)) =
    let i = a + e
        j = b + f
        k = c + g
        l = d + h
    in Matrix (i, j, k, l)

initFibMatrix :: Matrix
initFibMatrix = Matrix (1, 1, 1, 0)

fib4 :: Integer -> Integer
-- special case for n = 0
fib4 0 = 0
fib4 n =
  let prodMatrix = initFibMatrix ^ n
      -- we only care about the NE entry (where Fn is located)
      Matrix (_, fibn, _, _) = prodMatrix
  in fibn
