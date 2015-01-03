module Main where

import Scan

main :: IO ()
main = do
  s <- readFile "SolarTimes52xx.bas"
  print $ scan s
