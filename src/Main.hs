module Main where

import Utils (readLines)
import Day4.PassportProcessing

main :: IO ()
main = do
  r <- readLines "./data/day4.txt"
  let l = passportProcessing r
  print l -- $ take 3 l

