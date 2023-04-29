module Year2021.Day5 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.List (concatMap, filter, fromFoldable, singleton)
import Data.String as String
import Data.Pos (Pos(..))
import Util (parseInt, sign, dedupCountPrimitive)

type Line = Pos /\ Pos

intoTuple :: forall a. Array a -> Maybe (a /\ a)
intoTuple [a,b] = Just $ a /\ b
intoTuple _ = Nothing

intoTupleEither :: forall a. Show a => Array a -> String |? (a /\ a)
intoTupleEither arr = case intoTuple arr of
  Just t -> Right t
  Nothing -> Left $ "Expected array with 2 elements, got: " <> show arr

tupleToPos :: Int /\ Int -> Pos
tupleToPos (x /\ y) = Pos x y

parsePoint input = map tupleToPos <<< intoTupleEither =<< traverse parseInt (String.split (String.Pattern ",") input)
parseLine input = intoTupleEither =<< traverse parsePoint (String.split (String.Pattern " -> ") input)
parse = traverse parseLine <<< Array.filter (not <<< String.null) <<< String.split (String.Pattern "\n")

genLine :: Line -> List Pos
genLine ((Pos x1 y1) /\ (Pos x2 y2)) | x1 == x2 && y1 == y2 = singleton (Pos x1 y1)
genLine ((Pos x1 y1) /\ (Pos x2 y2)) = (Pos x1 y1) : genLine ((Pos (x1 + sign (x2 - x1)) (y1 + sign (y2 - y1))) /\ (Pos x2 y2))

isDiagonal :: Line -> Boolean
isDiagonal ((Pos x1 y1) /\ (Pos x2 y2)) = x1 /= x2 && y1 /= y2

computeScore :: List Pos -> Int
computeScore points = Array.length $ Array.filter (\(_ /\ c) -> c >= 2) $ dedupCountPrimitive $ Array.fromFoldable points

solvePartOne = computeScore <<< concatMap genLine <<< filter (not <<< isDiagonal)
solvePartTwo = computeScore <<< concatMap genLine

partOne input = parse input <#> fromFoldable >>> solvePartOne >>> show
partTwo input = parse input <#> fromFoldable >>> solvePartTwo >>> show

