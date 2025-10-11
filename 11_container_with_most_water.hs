module ContainerWithMostWater where

maxArea :: [Int] -> Int
maxArea heights
  | length heights < 2 = 0
  | otherwise = go 0 (length heights - 1) 0
  where
    go left right maxA
      | left >= right = maxA
      | otherwise =
          let area = min (heights !! left) (heights !! right) * (right - left)
              newMax = max maxA area
          in if heights !! left < heights !! right
             then go (left + 1) right newMax
             else go left (right - 1) newMax