import Data.List (foldl')

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
getHeight Leaf = -1
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
