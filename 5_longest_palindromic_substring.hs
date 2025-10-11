module LongestPalindrome where

import Data.List (inits, tails)

longestPalindrome :: String -> String
longestPalindrome s
  | null s = ""
  | otherwise = maximumBy (\x y -> compare (length x) (length y)) palindromes
  where
    palindromes = [sub | sub <- concatMap tails (inits s), sub == reverse sub]

maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ [x] = x
maximumBy cmp (x:xs) = go x xs
  where
    go m [] = m
    go m (y:ys) = go (if cmp m y == GT then m else y) ys