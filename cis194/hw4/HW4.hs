import Data.List (foldl', (\\))

-- Exercise 1

{-- NOTE: I don't agree that this is a good exercise
 - because the way the functions are already written is easy to read and to
 - understand. Using built-in functions would be cool, but I would not agree
 - that it would be 'better'. If one was to write a mathematical definition,
 - it would be similar to the original implementation of the functions.
 - My final rant/reason is that this exercise does not fit the 'Boring'
 - philosophy of this project!
--}

-- Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- NOTE: the provided solution tree does NOT have any ordering scheme
-- e.g. left child value < parent value < right child value

foldTree :: [a] -> Tree a
foldTree list = foldl' (flip insert) Leaf list

getHeight :: Tree a -> Integer
getHeight Leaf = -1 -- a sentinel value (-Inf would be more correct)
getHeight (Node height _ _ _) = height

insert :: a -> Tree a -> Tree a
insert element Leaf = Node 0 Leaf element Leaf
insert element (Node currHeight lT currElem rT) =
  let (lH, rH)         = (getHeight lT, getHeight rT)
      shouldInsertLeft = (min lH rH) == lH
      (newLT, newRT)   = if shouldInsertLeft
        then ((insert element lT), rT) else (lT, (insert element rT))
      childMaxHeight  = max (getHeight newLT) (getHeight newRT)
  in Node (childMaxHeight + 1) newLT currElem newRT

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node height lT _ rT) =
  let lrBalanced = (isBalanced lT) && (isBalanced rT)
      (lH, rH) = (getHeight lT, getHeight rT)
      currBalanced = (abs (lH - rH)) <= 1
  in lrBalanced && currBalanced

-- Exercise 3

myFoldl :: (a -> b -> a) -> a -> [b] -> a
{-- foldl is like foldr EXCEPT
 - it goes from left to right
 - ---> hence the `reverse`
 - its accumulation function is f(acc, elem) instead of f(elem, acc)
 - ---> hence the `flip`
--}
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Exercise 4 --

-- a function to get an i and j into the form used in the sieve
formSundaram :: Integer -> Integer -> Integer
formSundaram i j = i + j + 2 * i * j

-- the same function as above, but works on
-- `(i, j)` [a pair]
-- instead of
-- `i j` [two integers]
-- to you, it might not seem like a difference, but to Haskell, it is!
formSundaram' :: (Integer, Integer) -> Integer
formSundaram' = uncurry (formSundaram)

-- This is the two-part sieve filter function as described in the Wikipedia
-- article. Notice that it takes an integer `n` as the first argument.
-- This allows us to use the upper bound of the range in this second test.
-- In the main sieve code below, notice how we curry this function with the
-- `n` value given to the sieve function. "Currying" is an important technique
-- in Haskell. Learning about Mr. Haskell Curry and the `curry` and `uncurry`
-- functions will help you get stronger with Haskell!
filterSundaram :: Integer -> (Integer, Integer) -> Bool
filterSundaram n (i, j) =
  let s     = formSundaram i j
      test1 = 1 <= i && i <= j
      test2 = s <= n
  in test1 && test2

-- our main sieve function, as described in the exercise
sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let range     = [1..n]
      -- using the provided hint for the Cartesian product
      allPairs  = [(i, j) | i <- range, j <- range]
      -- get the list of pairs we want to remove
      -- (see comment about currying above)
      toRemovePairs  = filter (filterSundaram n) allPairs
      -- change all the (i, j) pairs to their "Sundaram" forms
      toRemoveIntegers = map (formSundaram') toRemovePairs
      -- take the difference of the range and the numbers we wish to remove
      remainingIntegers = range \\ toRemoveIntegers
  -- double and increment the remaining numbers to get primes up to 2n + 2
  in  map (\ k -> 2*k + 1)remainingIntegers
