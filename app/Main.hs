module Main where

import Day1
import Day2
import System.Environment

main = do
  args <- getArgs
  case args of
    ["1"] -> day1
    ["2"] -> day2
    _ -> putStrLn "Usage: cabal run aoc2023 -- [day #]"