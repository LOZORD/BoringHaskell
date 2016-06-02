-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits' n []

toDigits' :: Integer -> [Integer] -> [Integer]
toDigits' 0 ns = ns
toDigits' n ns =
  let (newN, digit) = n `divMod` 10
  in toDigits' newN (digit:ns)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Exercise 2
-- we have to do this "double reversal" in order to evaluate the list in
-- the natural order, i.e. left to right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = reverse (doubleEveryOtherLeftToRight (reverse list))

-- `cycle someList` takes a list and repeats it infinitely
doubleEveryOtherLeftToRight list = multiplyPairs (list) (cycle [1, 2])
  -- zipWith takes 2 lists and creates a new list of paired elements
  where multiplyPairs = zipWith (*)
