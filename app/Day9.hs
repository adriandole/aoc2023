module Day9 where

import Common
import Data.Either (fromRight)
import Data.List
import Data.Maybe (isJust)
import Data.Ord
import Data.Text (pack)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Printf

pNeg :: Parser Int
pNeg = do
  neg <- isJust <$> Text.Megaparsec.optional (char '-')
  num <- decimal
  return $ if neg then -1 * num else num

pHist :: Parser [[Int]]
pHist = (pNeg `sepBy` char ' ') `sepBy` newline

predict :: [Int] -> Int
predict xs =
  let diff = zipWith (-) (tail xs) xs
   in if all (== 0) xs
        then 0
        else last xs + predict diff

extrapolate :: [Int] -> Int
extrapolate xs =
  let diff = zipWith (-) (tail xs) xs
   in if all (== 0) xs
        then 0
        else head xs - extrapolate diff

day9 :: IO ()
day9 = do
  f <- readFile "input/day9.txt"
  let preds = fromRight (error "Parse error") (parse pHist "day9.txt" (pack f))
  printf "Part 1: %d\n" $ sum $ map predict preds
  printf "Part 2: %d\n" $ sum $ map extrapolate preds