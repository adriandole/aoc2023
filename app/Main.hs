module Main where

import Control.Lens
import Data.Text (pack)
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import System.Environment
import Text.Printf

main = do
  args <- getArgs
  let prefix = case args ^? element 1 of
        Just "test" -> "input/test/day"
        _  -> "input/day"
  fs <- readFile $ prefix ++ head args ++ ".txt"
  let f = pack fs
  let fns = [day1, day2, day3, day4, day5, day6, day7, day8, day9]
  let day = read (head args) :: Int
  case fns ^? element (day - 1) of
    Just fn -> fn f
    Nothing -> printf "Day %d not implemented" day