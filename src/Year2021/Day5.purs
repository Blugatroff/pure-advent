module Year2021.Day5 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.List (concatMap, filter, fromFoldable, length, singleton)
import Data.String as String
import Util (dedupCount, parseInt, sign)

type Point = Int /\ Int
type Line = Point /\ Point

intoTuple :: forall a. Array a -> Maybe (a /\ a)
intoTuple [a,b] = Just $ a /\ b
intoTuple _ = Nothing

intoTupleEither :: forall a. Show a => Array a -> String |? (a /\ a)
intoTupleEither arr = case intoTuple arr of
  Just t -> Right t
  Nothing -> Left $ "Expected array with 2 elements, got: " <> show arr

parsePoint input = intoTupleEither =<< traverse parseInt (String.split (String.Pattern ",") input)
parseLine input = intoTupleEither =<< traverse parsePoint (String.split (String.Pattern " -> ") input)
parse = traverse parseLine <<< Array.filter (not <<< String.null) <<< String.split (String.Pattern "\n")

genLine :: Line -> List Point
genLine ((x1 /\ y1) /\ (x2 /\ y2)) | x1 == x2 && y1 == y2 = singleton (x1 /\ y1)
genLine ((x1 /\ y1) /\ (x2 /\ y2)) = (x1 /\ y1) : genLine (((x1 + sign (x2 - x1)) /\ (y1 + sign (y2 - y1))) /\ (x2 /\ y2))

isDiagonal :: Line -> Boolean
isDiagonal ((x1 /\ y1) /\ (x2 /\ y2)) = x1 /= x2 && y1 /= y2

computeScore :: List Point -> Int
computeScore points = length $ filter (\(_ /\ c) -> c >= 2) $ dedupCount points

solvePartOne = computeScore <<< concatMap genLine <<< filter (not <<< isDiagonal)
solvePartTwo = computeScore <<< concatMap genLine

partOne input = parse input <#> fromFoldable >>> solvePartOne >>> show
partTwo input = parse input <#> fromFoldable >>> solvePartTwo >>> show

