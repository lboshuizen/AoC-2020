module Day5.BinaryBoarding (binaryBoarding) where

import Data.List (sort, foldl')

seatId :: String -> Int
seatId = foldl' (\a c -> 2 * a + (zo c)) 0
    where zo 'B' = 1
          zo 'R' = 1
          zo _ = 0 

pair :: [a] -> [(a,a)]
pair xs = zip xs (tail xs)

binaryBoarding :: [String] -> Int
binaryBoarding = (+1) . fst . head . filter (\(a,b) -> b-a == 2 ) . pair . sort . map seatId
