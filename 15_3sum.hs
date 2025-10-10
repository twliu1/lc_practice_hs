module ThreeSum where

threeSum :: [Int] -> Int -> [(Int, Int, Int)]
threeSum nums target = [(a, b, c) | (i, a) <- inums, (j, b) <- inums, (k, c) <- inums, i < j, j < k, a + b + c == target]
  where
    inums = zip [0..] nums