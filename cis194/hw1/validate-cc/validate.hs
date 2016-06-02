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

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits list =
  let
      -- first create a list of lists of digits
      -- it will look like [[1,6],[7],[1,2],[5]] using the example
      digitsList = map (toDigits) list
      -- then sum up all of the sublists
      -- using the same example, we should get [7, 7, 3, 5]
      digitSums  = map (sum) digitsList
  in sum digitSums -- then, all we need to do is just sum the main list!

-- Exercise 4
validate :: Integer -> Bool
validate creditCardNumber =
  let digits = toDigits creditCardNumber
      doubledEveryOtherDigits = doubleEveryOther digits
      sumResult = sumDigits doubledEveryOtherDigits
      modRemainder = sumResult `mod` 10
  in modRemainder == 0
