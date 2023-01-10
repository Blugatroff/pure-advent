module Year2022.Day5 (partOne, partTwo) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (Replacement(..), Pattern(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (foldl, traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
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

parseMove :: String -> Either Error Move
parseMove line = line
  # String.replace (Pattern "move") (Replacement "")
  # String.replace (Pattern "from") (Replacement "")
  # String.replace (Pattern "to") (Replacement "")
  # String.split (Pattern " ")
  # Array.filter (notEq "")
  # traverse parseInt
  >>= case _ of
    [amount, source, destination] -> Right { amount, source, destination }
    _ -> Left $ error $ "Failed to parse move from line: " <> line

parseMoves :: List String -> Either Error (List Move)
parseMoves = List.filter (notEq "") >>> traverse parseMove

parse :: String -> Either Error (Tuple (M.Map Int Stack) (List Move))
parse input = case splitOnce "" $ List.fromFoldable $ lines input of
  Nothing -> Left $ error "failed to find seperation between stacks and moves"
  Just (Tuple stacks s) -> do
    moves <- parseMoves s
    let stackMap = M.fromFoldable $ mapWithIndex (\i v -> Tuple (i + 1) v) $ parseStacks $ (transposeStacks $ Array.fromFoldable stacks)
    pure $ Tuple stackMap moves


applyMove :: (Array Char -> Array Char) -> M.Map Int Stack -> Move -> M.Map Int Stack
applyMove reverseOrNot stacks move = case Tuple (M.lookup move.source stacks) (M.lookup move.destination stacks) of
  Tuple (Just srcStack) (Just dstStack) -> 
    M.insert move.destination (reverseOrNot (Array.take move.amount srcStack) <> dstStack) stacks 
    # M.insert move.source (Array.drop move.amount srcStack)
  _ -> stacks

onTop :: M.Map Int Stack -> String
onTop stacks = (M.values stacks >>= (Array.take 1 >>> List.fromFoldable)) # Array.fromFoldable # fromCharArray

solve :: (Array Char -> Array Char) -> Tuple (M.Map Int Stack) (List Move) -> String
solve reverseOrNot (Tuple stacks moves) = onTop outputStacks
  where
    folder (Tuple i s) move = Tuple (i + 1) (applyMove reverseOrNot s move)

    (Tuple _ outputStacks) = foldl folder (Tuple 0 stacks) moves

solvePartOne :: Tuple (M.Map Int Stack) (List Move) -> String
solvePartOne = solve Array.reverse

solvePartTwo :: Tuple (M.Map Int Stack) (List Move) -> String
solvePartTwo = solve identity

partOne :: String -> Either Error String
partOne input = parse input <#> solvePartOne

partTwo :: String -> Either Error String
partTwo input = parse input <#> solvePartTwo
