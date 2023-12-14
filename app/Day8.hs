{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Common
import Data.Either
import Data.HashMap.Strict as HashMap (HashMap, fromList, keys, lookup, (!))
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf

type GraphNodes = HashMap String (String, String)

pGraph :: Parser (String, GraphNodes)
pGraph = do
  instrs <- someTill anySingle newline <* space1
  nodes <- some $ do
    name <- label <* string " = ("
    l <- label <* string ", "
    r <- label <* string ")" <* space1
    return (name, (l, r))
  return (instrs, fromList nodes)
  where
    label = count 3 letterChar

stepsToMatch :: (String -> Bool) -> GraphNodes -> String -> Int -> String -> Int
stepsToMatch matcher nodes path steps startNode =
  if matcher startNode
    then steps
    else
      let (l, r) = nodes ! startNode
          next = case head path of
            'L' -> l
            'R' -> r
       in stepsToMatch matcher nodes (tail path) (steps + 1) next

day8 :: Text -> IO ()
day8 f = do
  let (path, nodes) = fromRight (error "Parse error") (parse pGraph "day8.txt" f)
  let p1 = stepsToMatch (== "ZZZ") nodes (cycle path) 0 "AAA"
  let steps = map (stepsToMatch (\n -> n !! 2 == 'Z') nodes (cycle path) 0) (filter (\n -> n !! 2 == 'A') (keys nodes))
  let p2 = foldl lcm 1 steps
  --   parseTest pGraph (pack f)
  printf "Part 1: %d\n" p1
  printf "Part 2: %d\n" p2