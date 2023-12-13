{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Common
import Data.Either
import Data.HashMap.Strict as HashMap (HashMap, fromList, lookup, keys)
import Data.Text (pack)
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

stepsToZ :: GraphNodes -> String -> Int -> String -> Int
stepsToZ nodes path steps startNode =
  if startNode !! 2 == 'Z'
    then steps
    else case HashMap.lookup startNode nodes of
      Nothing -> error $ "Invalid node: " ++ startNode
      Just (l, r) -> case head path of
        'L' -> stepsToZ nodes (tail path) (steps + 1) l
        'R' -> stepsToZ nodes (tail path) (steps + 1) r
        _ -> error "Invalid instruction"

multiWalk :: Int -> GraphNodes -> [String] -> String -> ([String], Int)
multiWalk steps nodes startNodes path =
  if all (\n -> n !! 2 == 'Z') startNodes
    then (startNodes, steps)
    else
      multiWalk
        (steps + 1)
        nodes
        ( map
            ( \n -> case HashMap.lookup n nodes of
                Nothing -> error $ "Invalid node: " ++ n
                Just (l, r) -> case head path of
                  'L' -> l
                  'R' -> r
            )
            startNodes
        )
        (tail path)

day8 = do
  f <- readFile "input/day8.txt"
  let (path, nodes) = fromRight (error "Parse error") (parse pGraph "day8.txt" (pack f))
  let (_, p1) = multiWalk 0 nodes ["AAA"] (cycle path)
  let steps = map (stepsToZ nodes (cycle path) 0) (filter (\n -> n !! 2 == 'A') (keys nodes))
  let p2 = foldl lcm 1 steps
  --   parseTest pGraph (pack f)
  printf "Part 1: %d\n" p1
  printf "Part 2: %d\n" p2
--   printf "Part 2: %d\n" $ 0