module Main where

import Day11.SeatingSystem
import Utils (readLines)

main :: IO ()
main = do
  r <- readLines "./data/day11.txt"
  let l = seating r
  print l
