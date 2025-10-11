module MedianOfTwoSortedArrays where

medianSortedArrays :: [Int] -> [Int] -> Double
medianSortedArrays xs ys = case (totalLen, totalLen `mod` 2) of
  (0, _) -> 0.0
  (len, 0) -> fromIntegral (merged !! (len `div` 2 - 1) + merged !! (len `div` 2)) / 2.0
  (len, _) -> fromIntegral (merged !! (len `div` 2))
  where
    totalLen = length xs + length ys
    merged = merge xs ys
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys