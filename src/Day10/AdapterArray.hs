{-# LANGUAGE BangPatterns #-}

module Day10.AdapterArray (adapter) where

import Data.List (group, partition, sort)
import Utils (pairs)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

runlength :: [Int] -> [(Int, Int)]
runlength = map (\xs -> (head xs, length xs)) . group

distance :: [Int] -> [Int]
distance = map delta . pairs . sort
  where
    delta (a, b) = b - a

tribFib :: [Int]
tribFib = [1, 1, 2, 4, 7, 13, 24, 44, 81, 149]

groupByRunlengthOfDistances :: [Int] -> ([(Int, Int)], [(Int, Int)])
groupByRunlengthOfDistances = partition (\(n, _) -> n == 1) . runlength . distance . prepend0
  where
    prepend0 xs = 0 : xs

part1 = mult . both (sum . map snd) . groupByRunlengthOfDistances
  where
    mult (a, b) = a * (b + 1) -- +1 because input was NOT suffixed with max(xs)

arrangements = map ((tribFib !!) . snd)

part2 = product . arrangements . fst . groupByRunlengthOfDistances

adapter :: [Int] -> (Int, Int)
adapter xs = (a, b)
  where
    a = part1 xs
    b = part2 xs
