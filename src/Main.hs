module Main where

import Day13.ShuttleSearch
import Utils (readLines)

main :: IO ()
main = do
  r <- readLines "./data/day13.txt"
  let l = shuttle r
  print l
