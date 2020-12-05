module Main where

import Utils (readLines)
import Day5.BinaryBoarding

main :: IO ()
main = do
  r <- readLines "./data/day5.txt"
  let l = binaryBoarding r
  print l -- $ take 3 l

