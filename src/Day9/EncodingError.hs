module Day9.EncodingError (encoding) where

import Data.List (nub)
import Utils (stoi)

sums :: (Eq a, Num a) => [a] -> [a]
sums xs = nub $ [ (l+r) | l <- xs, r <- xs, l /= r ]

windows :: Int -> [a] -> [[a]]
windows n xs
    | (length xs) < n = []
    | otherwise = take n xs : windows n (drop 1 xs)

isSumOfPred :: [Int] -> Bool
isSumOfPred xs = elem t (sums l)
    where (t:l) = reverse $ xs

part1 = last . head . dropWhile isSumOfPred . windows 26 . map stoi

encoding :: [String] -> Int
encoding = last . head . dropWhile isSumOfPred . windows 26 . map stoi
