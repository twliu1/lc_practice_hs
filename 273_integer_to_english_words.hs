module IntToEnglish where

import Data.List

numberToWords :: Int -> String
numberToWords 0 = "Zero"
numberToWords n = intercalate " " $ processGroups (groupByThousands n) scales
  where
    scales = ["", "Thousand", "Million", "Billion"]

groupByThousands :: Int -> [Int]
groupByThousands 0 = []
groupByThousands n = n `mod` 1000 : groupByThousands (n `div` 1000)

processGroups :: [Int] -> [String] -> [String]
processGroups [] _ = []
processGroups (g:gs) (s:ss) = 
  case convertHundreds g of
    "" -> processGroups gs ss
    w  -> (w ++ if null s then "" else " " ++ s) : processGroups gs ss
processGroups _ [] = []

convertHundreds :: Int -> String
convertHundreds n
  | n == 0    = ""
  | n < 20    = ones !! n
  | n < 100   = let (tens, units) = n `divMod` 10
                in unwords $ filter (not . null) [twenties !! (tens - 2), ones !! units]
  | otherwise = let (hundreds, rest) = n `divMod` 100
                in unwords $ filter (not . null) [ones !! hundreds, "Hundred", convertHundreds rest]

ones :: [String]
ones = ["", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
        "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", 
        "Sixteen", "Seventeen", "Eighteen", "Nineteen"]

twenties :: [String]
twenties = ["Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"]
