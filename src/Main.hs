module Main where

import Scan
import Parse

main :: IO ()
main = do
  s <- readFile "SolarTimes52xx.bas"
  print $ parseFile $ scan s
