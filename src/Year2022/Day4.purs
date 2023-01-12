module Year2022.Day4 (partOne, partTwo) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Exception (Error, error)
import Util (lines, parseInt, splitStringOnce)

type Range = { start :: Int, end :: Int }

parseRange :: String -> Either Error Range
parseRange range = case splitStringOnce "-" range of
  Nothing -> Left $ error $ "failed to parse range: " <> range
  Just (Tuple left right) -> do
    start <- parseInt left
    end <- parseInt right
    pure { start, end }

parseLine :: String -> Either Error (Tuple Range Range)
parseLine line = case splitStringOnce "," line of
  Nothing -> Left $ error $ "Failed to parse line: " <> line
  Just (Tuple left right) -> do
    leftRange <- parseRange left
    rightRange <- parseRange right
    pure $ Tuple leftRange rightRange

parse :: String -> Either Error (Array (Tuple Range Range))
parse input = lines input <#> trim # Array.filter (notEq "") # traverse parseLine

rangeContains :: Range -> Range -> Boolean
rangeContains { start: ls, end: le } { start: rs, end: re } = ls <= rs && le >= re

rangesContainEachOther :: Range -> Range -> Boolean
rangesContainEachOther a b = rangeContains a b || rangeContains b a

rangesOverlap :: Range -> Range -> Boolean
rangesOverlap { start: ls, end: le } { start: rs, end: re } =
  (rs >= ls && rs <= le)
    || (re >= ls && re <= le)
    || (ls >= rs && ls <= re)
    || (le >= rs && le <= re)

solvePartOne :: Array (Tuple Range Range) -> Int
solvePartOne lines = lines # Array.filter (uncurry rangesContainEachOther) # Array.length

solvePartTwo :: Array (Tuple Range Range) -> Int
solvePartTwo lines = lines # Array.filter (uncurry rangesOverlap) # Array.length

partOne :: String -> Either Error String
partOne input = parse input <#> solvePartOne <#> show

partTwo :: String -> Either Error String
partTwo input = parse input <#> solvePartTwo <#> show
