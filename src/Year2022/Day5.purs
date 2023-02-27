module Year2022.Day5 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.List as List
import Data.Map as M
import Data.String (Replacement(..), Pattern(..))
import Data.String as String
import Util (lines, parseInt, splitOnce)

transposeStacks :: Array String -> Array Stack
transposeStacks input = input
  <#> toCharArray
  # Array.transpose
  <#> Array.reverse
  <#> Array.filter (notEq ']')
  <#> Array.filter (notEq '[')
  <#> fromCharArray
  <#> String.trim
  # Array.filter (notEq "")
  <#> toCharArray

type Stack = Array Char

type Move = { amount :: Int, source :: Int, destination :: Int }

parseStacks :: Array (Array Char) -> Array Stack
parseStacks lines = lines <#> Array.drop 1 <#> Array.reverse

parseMove :: String -> String |? Move
parseMove line = line
  # String.replace (Pattern "move") (Replacement "")
  # String.replace (Pattern "from") (Replacement "")
  # String.replace (Pattern "to") (Replacement "")
  # String.split (Pattern " ")
  # Array.filter (notEq "")
  # traverse parseInt
  >>= case _ of
    [ amount, source, destination ] -> Right { amount, source, destination }
    _ -> Left $ "Failed to parse move from line: " <> line

parseMoves :: List String -> String |? (List Move)
parseMoves = List.filter (notEq "") >>> traverse parseMove

parse :: String -> String |? (M.Map Int Stack /\ List Move)
parse input = case splitOnce "" $ List.fromFoldable $ lines input of
  Nothing -> Left "failed to find seperation between stacks and moves"
  Just (stacks /\ s) -> do
    moves <- parseMoves s
    let stackMap = M.fromFoldable $ mapWithIndex (\i v -> (i + 1) /\ v) $ parseStacks $ (transposeStacks $ Array.fromFoldable stacks)
    pure $ stackMap /\ moves

applyMove :: (Array Char -> Array Char) -> M.Map Int Stack -> Move -> M.Map Int Stack
applyMove reverseOrNot stacks move = case M.lookup move.source stacks, M.lookup move.destination stacks of
  Just srcStack, Just dstStack ->
    M.insert move.destination (reverseOrNot (Array.take move.amount srcStack) <> dstStack) stacks
      # M.insert move.source (Array.drop move.amount srcStack)
  _, _ -> stacks

onTop :: M.Map Int Stack -> String
onTop stacks = (M.values stacks >>= (Array.take 1 >>> List.fromFoldable)) # Array.fromFoldable # fromCharArray

solve :: (Array Char -> Array Char) -> M.Map Int Stack /\ List Move -> String
solve reverseOrNot (stacks /\ moves) = onTop outputStacks
  where
  folder (i /\ s) move = (i + 1) /\ applyMove reverseOrNot s move

  (_ /\ outputStacks) = foldl folder (0 /\ stacks) moves

solvePartOne :: M.Map Int Stack /\ List Move -> String
solvePartOne = solve Array.reverse

solvePartTwo :: M.Map Int Stack /\ List Move -> String
solvePartTwo = solve identity

partOne :: String -> String |? String
partOne input = parse input <#> solvePartOne

partTwo :: String -> String |? String
partTwo input = parse input <#> solvePartTwo
