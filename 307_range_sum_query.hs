module RangeSumQuery where

import Data.List

type NumArray = [Int]

update :: Int -> Int -> NumArray -> NumArray
update a b xs = take a xs ++ [b] ++ drop (a + 1) xs

sumRange :: Int -> Int -> NumArray -> Int
sumRange a b xs = foldl' (+) 0 (slice a b xs)
  where
    slice a b = take (b - a + 1) . drop a