{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Either
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Printf

type Parser = Parsec Void Text

data Color = Red Int | Green Int | Blue Int deriving (Eq, Show)

pRGB :: Parser Color
pRGB = do
  n <- decimal
  char ' '
  color <- choice [string "red", string "green", string "blue"]
  optional (string ", ")
  return $ case color of
    "red" -> Red n
    "green" -> Green n
    "blue" -> Blue n

pRound = do
  r <- count' 1 3 pRGB
  optional (string "; ")
  return r

pGame :: Parser (Int, [[Color]])
pGame = do
  string "Game "
  gameNum <- decimal
  string ": "
  rounds <- many pRound
  return (gameNum, rounds)

data GameStats = GameStats
  { maxR :: Int,
    maxG :: Int,
    maxB :: Int
  }
  deriving (Show, Eq)

colorToStat :: GameStats -> Color -> GameStats
colorToStat stats color = case color of
  Red n -> stats {maxR = max (maxR stats) n}
  Green n -> stats {maxG = max (maxG stats) n}
  Blue n -> stats {maxB = max (maxB stats) n}

aggStats :: (Int -> Int -> Int) -> GameStats -> GameStats -> GameStats
aggStats f a b =
  GameStats
    { maxR = f (maxR a) (maxR b),
      maxG = f (maxG a) (maxG b),
      maxB = f (maxB a) (maxB b)
    }

minEachColor :: (Int, [[Color]]) -> GameStats
minEachColor g =
  let startStat = GameStats {maxR = 0, maxG = 0, maxB = 0}
   in let statsPerRound = map (foldl colorToStat startStat) (snd g)
       in foldl (aggStats max) startStat statsPerRound

day2 :: IO ()
day2 = do
  f <- readFile "input/day2.txt"
  let l = map pack (lines f)
  let forceParse line = fromRight (error "Parse error") (parse pGame "" line)
  let games = map forceParse l

  let validGame g = (maxR stat <= 12) && (maxG stat <= 13) && (maxB stat <= 14)
        where
          stat = minEachColor g
  let setPower g = (maxR stat) * (maxG stat) * (maxB stat)
        where
          stat = minEachColor g

  let validSum = sum [fst game | game <- games, validGame game]
  let powerSum = sum [setPower game | game <- games]
  printf "Part 1: %d\n" validSum
  printf "Part 2: %d\n" powerSum