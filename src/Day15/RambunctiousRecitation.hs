module Day15.RambunctiousRecitation (recitation) where

import qualified Data.IntMap.Strict as M
import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = map read . splitOn ","

part1 :: Int -> [Int] -> Int
part1 l xs = go (length xs -1) (last xs) seed
  where
    go i n m
      | i == (l -1) = n
      | otherwise = case m M.!? n of
        Nothing -> go (succ i) 0 (M.insert n i m)
        Just l -> go (succ i) (i - l) (M.insert n i m)
    seed = M.fromList $ zip (init xs) [0 ..]

recitation xs = (a, b)
  where
    ns = head . map parse $ xs
    a = part1 2020 ns
    b = part1 30000000 ns