module Day13.ShuttleSearch (shuttle) where

import Data.List (minimumBy, sortBy)
import Data.List.Split (splitWhen)
import Data.Ord (comparing)
import Utils (stoi)

parse1 :: [String] -> (Int, [(Int, Int)])
parse1 (l1 : l2 : _) = (stoi l1, ns l2)
  where
    ns = map (\(i, x) -> (i, stoi x)) . filter (\(_, x) -> x /= "x") . zip [0 ..] . splitWhen (== ',')

part1 :: Int -> [(Int, Int)] -> Int
part1 n = prod . minimumBy (comparing snd) . map (\(_, x) -> (x, x - n `mod` x))
  where
    prod (a, b) = a * b

part2 = 0

shuttle xs = (a, b)
  where
    (n, l) = parse1 xs
    a = part1 n l
    b = 0 -- WIP
