module SubstringWithoutRepeat where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')

longestSubstringLength :: String -> Int
longestSubstringLength s =
  let (best, _, _) = foldl' step (0, 0, Map.empty) (zip [0..] s)
  in best
  where
    step :: (Int, Int, Map Char Int) -> (Int, Char) -> (Int, Int, Map Char Int)
    step (best, start, seen) (i, c) =
      let start' = maybe start (\idx -> max start (idx + 1)) (Map.lookup c seen)
          curr   = i - start' + 1
          best'  = max best curr
          seen'  = Map.insert c i seen
      in (best', start', seen')
