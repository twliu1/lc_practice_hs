module TwoSum where

twoSum :: [Int] -> Int -> (Int, Int)
twoSum nums target = head [(i, j) | i <- [0..n-1], j <- [i+1..n-1], nums !! i + nums !! j == target]
  where n = length nums