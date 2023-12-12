module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import System.Environment

main = do
  args <- getArgs
  case args of
    ["1"] -> day1
    ["2"] -> day2
    ["3"] -> day3
    ["4"] -> day4
    ["5"] -> day5
    _ -> putStrLn "Usage: cabal run aoc2023 -- [day #]"