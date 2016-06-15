module Long where

import Data.List

-- Exercise 1
divides :: Int -> Int -> Bool
x `divides` y = y `mod` x == 0

index :: [a] -> [(Int, a)]
index = zip [1..]

getVal :: (Int, a) -> a
getVal (_, v) = v

unindex :: [(Int, a)] -> [a]
--unindex list = [a | (i, a) <- list]
unindex = map (getVal)

skips :: [a] -> [[a]]
skips list = map (unindex) $ skips' 1 (index list)

dividesIndex :: Int -> (Int, a) -> Bool
dividesIndex n (i, _) = n `divides` i

skips' :: Int -> [(Int, a)] -> [[(Int, a)]]
skips' n indexedList
  | n > (length indexedList) = []
  | otherwise =
    let newEntry = filter (dividesIndex n) indexedList
    in (newEntry:(skips' (n + 1) indexedList))

-- Exercise 2 -- TODO

{--
isValley :: Int -> [Integer] -> Bool
isValley n list
  | ((n == 0) || (n == length list)) = True
  | otherwise =
    let lft = list !! n - 1
        mid = list !! n
        rgt = list !! n + 1
    in lft >= mid && mid <= rgt

valleyGrouping :: [Integer] -> (Int, Integer) -> (Int, Integer) -> Bool
valleyGrouping list _ (i, _) = isValley i list

getPeakValues :: [(Int, Integer)] -> [(Int, Integer)]
getPeakValues range =
  let justVals = map (getVal) range
      peakValue = maximum justVals
  in  filter ((== peakValue).getVal) range

getPeaks :: [[(Int, Integer)]] -> [[(Int, Integer)]]
getPeaks valleys = map (getPeakValues) valleys

flatten :: [[a]] -> [a]
flatten listOfLists = foldl' (\ acc elm -> acc ++ elm) [] listOfLists

localMaxima :: [Integer] -> [Integer]
localMaxima list =
  let separatedByValleys = groupBy (valleyGrouping list) (index list)
      peaks = getPeaks separatedByValleys
      flattenedPeakValues = flatten peaks
  in unindex flattenedPeakValues
--}
{--
isPeak :: Int -> [Integer]
isPeak 0 _ = False
isPeak n list =
  | n == (length list) = False
  | otherwise =
    let lft = list !! (n - 1)
        mid = list !! n
        rgt = list !! (n + 1)
    in lft < mid && mid > rgt

localMaxima :: [Integer] -> [Integer]
localMaxima list = localMaxima' 0 (length list) list

localMaxima' start end list =
  let midInd = (start + end) `div` 2
  in if isPeak midInd list
    then undefined
    else undefined
--}

{--
localMaxima :: [Integer] -> [Integer]
localMaxima list = localMaxima' 0 (length list) list

localMaxima' :: Int -> Int -> [Integer]
localMaxima' start end list =
  if start == end
    then [list !! start]
    else if start + 2 == end
          then
            let left = list !! start
                middle = list !! start + 1
                right = list !! end
             in if left < middle && middle < right then [middle] else []
          else
            let leftMaxima = localMaxima' start (end - 1) list
                rightMaxima = localMaxima' (start + 1) end list
                middleMaxima = localMaxima' (start + 1) (end - 1) list
            in leftMaxima ++ rightMaxima ++ middleMaxima
--}
