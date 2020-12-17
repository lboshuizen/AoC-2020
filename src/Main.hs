module Main where

import           Day17.ConwayCubes (boot)
import           Utils             (readLines)

main :: IO ()
main = do
  r <- readLines "./data/day17.txt"
  let l = boot r
  print l
