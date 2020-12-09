module Main where

import Utils (readLines)
import Day9.EncodingError

main :: IO ()
main = do
  r <- readLines "./data/day9.txt"
  let l = encoding r
  print l
