module TrappingRainWater where

trappingRainWater :: [Int] -> Int
trappingRainWater heights
  | length heights < 3 = 0
  | otherwise = sum $ zipWith3 (\l r h -> max 0 (min l r - h)) leftMax rightMax heights
  where
    leftMax = tail $ scanl max 0 heights
    rightMax = init $ scanr max 0 heights