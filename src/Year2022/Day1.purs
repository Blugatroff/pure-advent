module Year2022.Day1 (partOne, partTwo) where

import Prelude

import Data.Either (Either)
import Data.Foldable (maximum, sum)
import Data.List (List)
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Effect.Exception (Error)
import Util (lines, parseInt, split)

parse :: String -> Either Error (List (List Int))
parse input =
  let
    groups = split "" $ List.fromFoldable $ lines input
  in
    traverse (traverse parseInt) groups

solvePartOne :: List (List Int) -> Int
solvePartOne = map sum >>> maximum >>> fromMaybe (-1)

solvePartTwo :: List (List Int) -> Int
solvePartTwo = map sum >>> List.sort >>> List.takeEnd 3 >>> sum

partOne :: String -> Either Error String
partOne input = parse input <#> solvePartOne <#> show

partTwo :: String -> Either Error String
partTwo input = parse input <#> solvePartTwo <#> show
