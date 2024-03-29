module Year2022.Day4 (day) where

import MeLude

import Data.Array as Array
import Data.String (trim)
import Day (makeDay)
import Util (lines, parseInt, splitStringOnce)

type Range = { start :: Int, end :: Int }

parseRange :: String -> String |? Range
parseRange range = case splitStringOnce "-" range of
  Nothing -> Left $ "failed to parse range: " <> range
  Just (left /\ right) -> do
    start <- parseInt left
    end <- parseInt right
    pure { start, end }

parseLine :: String -> String |? (Range /\ Range)
parseLine line = case splitStringOnce "," line of
  Nothing -> Left $ "Failed to parse line: " <> line
  Just (left /\ right) -> do
    leftRange <- parseRange left
    rightRange <- parseRange right
    pure $ leftRange /\ rightRange

parse :: String -> String |? (Array (Range /\ Range))
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

solvePartOne :: Array (Range /\ Range) -> Int
solvePartOne lines = lines # Array.filter (uncurry rangesContainEachOther) # Array.length

solvePartTwo :: Array (Range /\ Range) -> Int
solvePartTwo lines = lines # Array.filter (uncurry rangesOverlap) # Array.length

day = makeDay parse
  (Right <<< show <<< solvePartOne)
  (Right <<< show <<< solvePartTwo)
