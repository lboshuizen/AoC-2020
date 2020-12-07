module Main where

import Utils (readLines)
import Day7.HandyHaversacks

main :: IO ()
main = do
  r <- readLines "./data/day7.txt"
  print (head r)
  let l = haversacks r
  print l
