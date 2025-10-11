module MaxSubarray where

maxSubarray :: [Int] -> Int
maxSubarray [] = 0
maxSubarray xs = maximum $ scanl1 (\acc x -> max x (acc + x)) xs