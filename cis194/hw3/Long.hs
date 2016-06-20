module Long where

import Data.List

-- Exercise 1
divides :: Int -> Int -> Bool
x `divides` y = y `mod` x == 0

indexFrom :: Int -> [a] -> [(Int, a)]
indexFrom start = zip [start..]

index :: [a] -> [(Int, a)]
index = indexFrom 1

getVal :: (Int, a) -> a
getVal (_, v) = v

unindex :: [(Int, a)] -> [a]
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

-- Exercise 2 --

localMaxima :: [Integer] -> [Integer]
localMaxima list = localMaxima' list []

-- we take three elements at a time and see if they form a peak
localMaxima' :: [Integer] -> [Integer] -> [Integer]
localMaxima' list maxima
  -- no peaks in that short of a list
  | (length list) < 3 = maxima
  -- otherwise, we might have a potential peak
  | otherwise =
    let [left, middle, right] = take 3 list
        isPeak = left < middle && middle > right
        -- update the maxima list if we have a peak
        newMaxima = if isPeak then (maxima ++ [middle]) else maxima
        -- recurse on the rest of the list
    in localMaxima' (tail list) newMaxima

-- Exercise 3 --

histogram :: [Integer] -> String
histogram nums =
  let counts = getCounts nums
      stars  = starify counts
      bordered = stars ++ ["==========", "0123456789"]
  -- join all result rows together with newlines , and add newline at the end
  in (intercalate "\n" bordered) ++ "\n"

updateCounts :: Integer -> [Integer] -> [Integer]
-- given a number and a current list of counts (how much of each number we've seen)
-- use that number parameter as an index; increment the count at that index
updateCounts ind counts = map (\(i,c) -> if i == (fromIntegral ind) then c + 1 else c) (indexFrom 0 counts)

getCounts :: [Integer] -> [Integer]
-- run `updateCounts` over an initial list of 10 zeroes
getCounts = foldr (updateCounts) (replicate 10 0)

starify :: [Integer] -> [String]
starify counts
  -- if there are no more star rows to add, we're done
  | all (==0) counts = []
  | otherwise =
        -- otherwise, get a new row
    let newRow = map (\ c -> if c > 0 then '*' else ' ') counts
        -- and get a new representation of the histogram data counts
        newCounts = map (\ c -> if c > 0 then c - 1 else 0) counts
    in (starify newCounts) ++ [newRow]
