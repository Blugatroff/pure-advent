module Year2022.Day12 (partOne, partTwo) where

import Prelude

import Data.Array (catMaybes)
import Data.Array as Array
import Data.CodePoint.Unicode (isAsciiLower)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.List (List)
import Data.List as List
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (minimum, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Dijkstra (class World, Cell(..), Solution(..), adjacentCells, findSolutionFrom)
import Effect.Exception (Error, error)
import Util (indexed, lines, trace)

data HeightMapCell = Start | End | Height Int

derive instance eqHeightMapCell :: Eq HeightMapCell

newtype HeightMap = HeightMap (M.Map (Tuple Int Int) HeightMapCell)

maximumHeight :: Int
maximumHeight = fromEnum 'z' - fromEnum 'a'

cellHeight :: HeightMapCell -> Int
cellHeight (Height h) = h
cellHeight End = maximumHeight
cellHeight Start = 0

instance World HeightMap (Tuple Int Int) where
  lookupCell (Tuple x y) (HeightMap map) = M.lookup (Tuple x y) map <#> toDijkstraCell
    where
    toDijkstraCell :: HeightMapCell -> Cell
    toDijkstraCell Start = Cell 1
    toDijkstraCell End = Destination 1
    toDijkstraCell (Height h) = Cell 1

  adjacentCells (Tuple x y) (HeightMap world) =
    [ Tuple (x - 1) y, Tuple (x + 1) y, Tuple x (y - 1), Tuple x (y + 1) ]
      # Array.filter movePossible
    where
    movePossible to = case Tuple (M.lookup (Tuple x y) world) (M.lookup to world) of
      (Tuple (Just previous) (Just this)) -> cellHeight previous + 1 >= cellHeight this
      (Tuple Nothing (Just _)) -> true
      _ -> false

parseCell :: Char -> Either Error HeightMapCell
parseCell 'S' = Right Start
parseCell 'E' = Right End
parseCell c | isAsciiLower $ codePointFromChar c = Right $ Height $ fromEnum c - fromEnum 'a'
parseCell c = Left $ error $ "failed to parse cell " <> show c

parseLine :: String -> Either Error (Array HeightMapCell)
parseLine = toCharArray >>> traverse parseCell

parse :: String -> Either Error HeightMap
parse input = lines input <#> String.trim # Array.filter (not <<< String.null) # traverse parseLine <#> assocs <#> M.fromFoldable <#> HeightMap
  where
  assocs :: Array (Array HeightMapCell) -> Array (Tuple (Tuple Int Int) HeightMapCell)
  assocs lines = indexed lines >>= (\(Tuple y l) -> indexed l <#> \(Tuple x c) -> Tuple (Tuple x y) c)

findHeightMapStart :: HeightMap -> Maybe (Tuple Int Int)
findHeightMapStart (HeightMap map) = M.toUnfoldable map # Array.filter (eq Start <<< snd) # Array.head <#> fst

solvePartOne :: HeightMap -> Maybe Int
solvePartOne heightMap = do
  (Tuple x y) <- findHeightMapStart heightMap
  { cost } <- findSolutionFrom heightMap (Tuple x y)
  Just $ cost - 1

solvePartTwo :: HeightMap -> Maybe Int
solvePartTwo heightMap@(HeightMap hm) = minimum (_.cost <$> solutions) <#> sub 1
  where
  startingPositions = M.toUnfoldable hm # Array.filter (snd >>> cellHeight >>> eq 0) <#> fst

  solutions :: Array (Solution (Tuple Int Int))
  solutions = startingPositions <#> findSolutionFrom heightMap # catMaybes

partOne :: String -> Either Error String
partOne input = parse input <#> solvePartOne <#> show

partTwo :: String -> Either Error String
partTwo input = parse input <#> solvePartTwo <#> show
