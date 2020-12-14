module Main where

import Day14.DockingData
import Utils (readLines)

main :: IO ()
main = do
  r <- readLines "./data/day14.txt"
  let l = docking r
  print l
