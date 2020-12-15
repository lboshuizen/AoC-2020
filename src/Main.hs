module Main where

import Day15.RambunctiousRecitation
import Utils (readLines)

main :: IO ()
main = do
  r <- readLines "./data/day15.txt"
  let l = recitation r
  print l
