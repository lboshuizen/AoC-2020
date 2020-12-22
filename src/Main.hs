module Main where

import           Day22.CrabCombat (combat)
import           Utils            (readLines)

main :: IO ()
main = do
  r <- readLines "./data/day22.txt"
  let l = combat r
  print l
