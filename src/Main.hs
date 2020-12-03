module Main where

import Utils (readLines)
import Day3.TobogganTrajectory

slopesPart1 = [ (3,1) ]
slopesPart2 = [ (1,1), (3,1), (5,1), (7,1), (1,2) ]

main :: IO ()
main = do
  r <- readLines "./data/day3.txt"
  let l = tobogganTrajectory r slopesPart2
  print l

