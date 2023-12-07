module Day1 where

import Data.Char
import Data.List
import Data.Maybe
import Text.Printf

convertSpelled :: String -> String
convertSpelled [] = []
convertSpelled s =
  let digits = zip [1 ..] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
   in let matchingSpell = find (\d -> isPrefixOf (snd d) s) digits
       in case matchingSpell of
            Just (i, iSpelled) -> show i ++ (convertSpelled $ tail s)
            Nothing -> head s : convertSpelled (tail s)

calibrationScore :: String -> Int
calibrationScore line =
  let digitsOnly = filter isDigit line
   in let n = (head digitsOnly) : [last digitsOnly]
       in read n :: Int

day1 :: IO ()
day1 = do
  f <- readFile "input/day1.txt"
  let l = lines f
  printf "Part 1: %d\n" $ sum $ map calibrationScore l
  printf "Part 2: %d\n" $ sum $ map (calibrationScore . convertSpelled) l