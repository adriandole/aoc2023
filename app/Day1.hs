module Day1 where

import Data.Char
import Data.List
import qualified Data.Text as T
import Data.Maybe
import Text.Printf

convertSpelled :: String -> String
convertSpelled [] = []
convertSpelled s =
  let digits = zip [1 ..] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      matchingSpell = find (\d -> snd d `isPrefixOf` s) digits
   in case matchingSpell of
        Just (i, iSpelled) -> show i ++ convertSpelled (tail s)
        Nothing -> head s : convertSpelled (tail s)

calibrationScore :: String -> Int
calibrationScore line =
  let digitsOnly = filter isDigit line
   in let n = head digitsOnly : [last digitsOnly]
       in read n :: Int

day1 :: T.Text -> IO ()
day1 f = do
  let l = lines (T.unpack f)
  printf "Part 1: %d\n" $ sum $ map calibrationScore l
  printf "Part 2: %d\n" $ sum $ map (calibrationScore . convertSpelled) l