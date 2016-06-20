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
