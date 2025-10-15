module SortColors where

sortColors :: [Int] -> [Int]
sortColors xs = concat $ zipWith replicate counts [0, 1, 2]
  where
    counts = [length $ filter (== n) xs | n <- [0, 1, 2]]
