module Main where

import Utils
import Day1.ReportRepair

main :: IO ()
main = do
  r <- readLines "./data/day1.txt"
  let xs = map stoi r
  let r = reportRepair 2020 xs  
  print r