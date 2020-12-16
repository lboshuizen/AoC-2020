module Main where

import Day16.TicketTranslation
import Utils (readLines)

main :: IO ()
main = do
  r <- readLines "./data/day16.txt"
  let l = ticket r
  print l
