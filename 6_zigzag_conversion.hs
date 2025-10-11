module ZigZagConversion where

import Data.List (sortOn, groupBy)

convert :: String -> Int -> String
convert s numRows
  | numRows <= 1 || null s = s
  | otherwise = concatMap (map snd) $ groupBy (\(r1,_) (r2,_) -> r1 == r2) $ sortOn fst indexed
  where
    cycleLength = 2 * (numRows - 1)
    rowIndices = concat $ repeat $ [0..numRows-1] ++ [numRows-2, numRows-3 .. 1]
    indexed = zip (take (length s) rowIndices) s