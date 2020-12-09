module Day9.EncodingError (encoding) where

import Data.List (inits, tails)
import Utils (stoi)

windows :: Int -> [a] -> [[a]]
windows n xs = [ take n . drop i $ xs | i <- [0..(length xs) - n] ]

lastIsSumOfPred :: [Int] -> Bool
lastIsSumOfPred = canSum . reverse
    where canSum (x:xs) = or [ l+r==x | l <- xs, r <- xs, l /= r ]

part1 = last . head . dropWhile lastIsSumOfPred . windows (25+1)

sweep :: Int -> [Int] -> [[Int]]
sweep n = dropWhile (\l -> sum l /= n) . concatMap tails . inits

part2 xs = min + max
    where
        n = part1 xs
        seq = head . sweep n $ xs
        (min,max) = (minimum seq, maximum seq)

encoding :: [String] -> Int
encoding = part2 . map stoi
