module Day3 where

import Common
import Data.Either
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.List (nub)
import Data.Text (pack)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Printf

data S = Number Int | Symbol Char | Dot deriving (Show)

data Sv = Sv
  { pos :: SourcePos,
    val :: S
  }
  deriving (Show)

pGrid :: Parser [Sv]
pGrid = many $ do
  space
  pos <- getSourcePos
  val <- choice [Number <$> decimal, Dot <$ char '.', Symbol <$> oneOf "@#$%&*+-=/"]
  return Sv {pos = pos, val = val}

symbolSet grid =
  let gridL sv = case sv of
        Sv pos (Symbol c) -> [(unPos $ sourceLine pos, unPos $ sourceColumn pos)]
        _ -> []
   in HashSet.fromList (concatMap gridL grid)

numberMap grid =
  let numberL sv = case sv of -- create a HashMap from pos to value
        Sv pos (Number n) ->
          let l = length (show n)
              i = unPos $ sourceLine pos
              j = unPos $ sourceColumn pos
           in [((i, j + x), n) | x <- [0 .. l - 1]]
        _ -> []
   in HashMap.fromList (concatMap numberL grid)

neighbors sv =
  let len = case sv of
        Sv pos (Number n) -> length $ show n
        _ -> 1
      p = pos sv
      i = unPos $ sourceLine p
      j = unPos $ sourceColumn p
      lr = [(i, j - 1), (i, j + len)]
      diag = [(i - 1, j - 1), (i + 1, j - 1), (i - 1, j + len), (i + 1, j + len)]
      vert = concat [[(i + 1, j + x), (i - 1, j + x)] | x <- [0 .. len - 1]]
   in lr ++ diag ++ vert

valid set num = any (`HashSet.member` set) (neighbors num)

nval sv = case sv of
  Sv _ (Number n) -> n
  _ -> 0

ratio set sv = case sv of
  Sv pos (Symbol '*') ->
    let numNeighbors = filter (`HashMap.member` set) (neighbors sv)
        neighborVals = nub (map (set HashMap.!) numNeighbors)
     in if length neighborVals == 2
          then product neighborVals
          else 0
  _ -> 0

day3 :: IO ()
day3 = do
  f <- readFile "input/day3.txt"
  let grid = fromRight (error "Parse error") (parse pGrid "day3.txt" (pack f))
  let sset = symbolSet grid
  let nmap = numberMap grid
  let p1 = sum $ map nval $ filter (valid sset) grid
  let p2 = sum $ map (ratio nmap) grid

  -- print $ sum $ map (ratio nmap) grid
  printf "Part 1: %d\n" p1
  printf "Part 2: %d\n" p2