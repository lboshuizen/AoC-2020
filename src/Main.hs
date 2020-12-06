module Main where

import Utils (readLines)
import Day6.CustomCustoms

main :: IO ()
main = do
  r <- readLines "./data/day6.txt"
  let l = customs r
  print l -- $ take 3 l

