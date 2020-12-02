module Main where

import Utils
import Day2.PasswordPhilosophy

main :: IO ()
main = do
  r <- readLines "./data/day2.txt"
  let l = length (passwordPhilosophy r)
  print l
