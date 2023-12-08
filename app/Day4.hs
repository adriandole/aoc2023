{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Common
import Data.Either
import Data.HashSet (HashSet, fromList, intersection)
import Data.Text (pack)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Printf

data Card = Card
  { num :: Int,
    winning :: HashSet Int,
    have :: HashSet Int
  }
  deriving (Eq, Show)

pCards :: Parser [Card]
pCards = many $ do
  string "Card"
  space1
  num <- decimal
  string ":"
  space1
  winning <- fromList <$> many (decimal <* space1)
  string "|"
  space1
  have <- fromList <$> many (decimal <* space1)
  return $ Card num winning have

matches card = length (intersection (winning card) (have card))

score :: Card -> Int
score card = if l > 0 then 2 ^ (l - 1) else 0
  where
    l = matches card

winN :: [Card] -> Int
winN [] = 0
winN (card : cards) =
  let wonCopies = [drop x cards | x <- [0 .. matches card - 1]]
   in 1 + sum (map winN wonCopies)

day4 = do
  f <- readFile "input/day4.txt"
  let cards = fromRight (error "Parse error") (parse pCards "day3.txt" (pack f))
  -- parseTest pCards (pack f)
  let p1 = sum $ map score cards
  let p2 = sum $ [winN (drop x cards) | x <- [0 .. length cards]]
  printf "Part 1: %d\n" p1
  printf "Part 2: %d\n" p2
