{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Common
import Data.Either
import Data.List
import Data.Ord (comparing)
import Data.Range
import Data.Text (pack)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Printf

type RangeI = Range Int

data RangeMap = RangeMap
  { srcRange :: RangeI,
    dstRange :: RangeI
  }
  deriving (Show)

pRange :: Parser RangeMap
pRange = do
  dstStart <- decimal <* space
  srcStart <- decimal <* space
  len <- decimal <* space
  return $ RangeMap (srcStart +=* (srcStart + len)) (dstStart +=* (dstStart + len))

pMap :: Parser [RangeMap]
pMap = do
  anySingle `manyTill` string "map:\n"
  many pRange

pAtlas :: Parser ([Int], [[RangeMap]])
pAtlas = do
  string "seeds: "
  seeds <- many (decimal <* space1)
  ranges <- many pMap
  return (seeds, ranges)

goMap :: Int -> [RangeMap] -> Int
goMap x ranges =
  case find (\r -> inRange (srcRange r) x) ranges of
    Just r -> x + rDiff r
    Nothing -> x

location :: [[RangeMap]] -> Int -> Int
location map seed = foldl goMap seed map

rangeStart :: RangeI -> Int
rangeStart r = case r of
  SpanRange (Bound a Inclusive) _ -> a
  SingletonRange a -> a
  _ -> error "Only SpanRange supported"

shiftRange :: Int -> RangeI -> RangeI
shiftRange x r = case r of
  SpanRange (Bound a ba) (Bound b bb) -> SpanRange (Bound (a + x) ba) (Bound (b + x) bb)
  SingletonRange a -> SingletonRange (a + x)
  _ -> error "Only SpanRange supported"

rDiff map = rangeStart (dstRange map) - rangeStart (srcRange map)

-- Apply a single map, only where there's an intersection
applyMap :: RangeI -> RangeMap -> [RangeI]
applyMap r m =
  let inter = intersection [r] [srcRange m]
   in map (shiftRange $ rDiff m) inter

applyMaps :: [RangeMap] -> [RangeI] -> [RangeI]
applyMaps maps input = srcUnmapped ++ shiftedRanges
  where
    mapSrcRanges = map srcRange maps
    srcUnmapped = intersection (invert mapSrcRanges) input
    -- for each source range, call applyMap on each map
    shiftedRanges = concatMap (\src -> concatMap (applyMap src) maps) input

traverseAllMaps :: [RangeI] -> [[RangeMap]] -> [RangeI]
traverseAllMaps = foldl (flip applyMaps)

seedLocations :: [[RangeMap]] -> [Int] -> [RangeI]
seedLocations _ [] = []
seedLocations maps (start:len:rs) = let
  seedR = start +=* (start + len)
  in
    traverseAllMaps [seedR] maps ++ seedLocations maps rs

minLocation :: [RangeI] -> Int
minLocation locs = minimum $ map rangeStart locs

day5 = do
  f <- readFile "input/day5.txt"
  let (seeds, maps) = fromRight (error "Parse error") (parse pAtlas "day5.txt" (pack f))
  -- parseTest pAtlas (pack f)
  let p1 = minimum $ map (location maps) seeds

  let p2 = minLocation $ seedLocations maps seeds
  print p2