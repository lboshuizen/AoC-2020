module Main where

import Utils (readLines)
import Day8.HandheldHalting

main :: IO ()
main = do
  r <- readLines "./data/day8.txt"
  let l = halting r
  print l
