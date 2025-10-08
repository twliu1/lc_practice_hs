module BestTime where

maximumOrZero :: [Int] -> Int
maximumOrZero xs = if null xs then 0 else maximum xs

maxProfit :: [Int] -> Int
maxProfit prices = maximumOrZero [sell - buy | (buy, sell) <- zip minPrices prices]
  where
    minPrices = scanl1 min prices