module Main where

import Day10.AdapterArray
import Utils (readLines, stoi)

main :: IO ()
main = do
  r <- readLines "./data/day10.txt"
  let l = adapter (map stoi r)
  print l
