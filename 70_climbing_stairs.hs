module ClimbingStairs where

climbStairs :: Int -> Int
climbStairs n = ways !! n
  where
    ways = 0 : 1 : 2 : zipWith (+) (drop 1 ways) (drop 2 ways)
