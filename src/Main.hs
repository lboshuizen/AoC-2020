module Main where

import Day12.RainRisk
import Utils (readLines)

main :: IO ()
main = do
  r <- readLines "./data/day12.txt"
  let l = rain r
  print l
