module SearchRotatedSortedArray where

search :: [Int] -> Int -> Int
search nums target = go nums target 0 (length nums - 1)
  where
    go :: [Int] -> Int -> Int -> Int -> Int
    go xs t lo hi
      | lo > hi = -1
      | otherwise =
          let mid = lo + (hi - lo) `div` 2
              x = xs !! mid
          in if x == t
             then mid
             else if xs !! lo <= x
                  then if xs !! lo <= t && t < x
                       then go xs t lo (mid - 1)
                       else go xs t (mid + 1) hi
                  else
                       if x < t && t <= xs !! hi
                       then go xs t (mid + 1) hi
                       else go xs t lo (mid - 1)