module MergeIntervals where

import Data.List
import Data.Ord

merge :: [(Int, Int)] -> [(Int, Int)]
merge = foldr mergeStep [] . sortBy (comparing fst <> comparing snd)
  where
    mergeStep (start, end) [] = [(start, end)]
    mergeStep (start1, end1) ((start2, end2):rest)
      | end1 >= start2 = (min start1 start2, max end1 end2) : rest
      | otherwise      = (start1, end1) : (start2, end2) : rest