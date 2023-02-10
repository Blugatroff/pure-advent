module Year2022.Day12 (partOne, partTwo) where

import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode (isAsciiLower)
import Data.Either (Either(..), note)
import Data.Enum (fromEnum)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Dijkstra (class World, Cell(..), findSolutionFrom)
import Effect.Exception (Error, error)
import Util (indexed, lines)

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
    toDijkstraCell (Height _) = Cell 1

  adjacentCells (Tuple x y) (HeightMap world) =
    [ Tuple (x - 1) y, Tuple (x + 1) y, Tuple x (y - 1), Tuple x (y + 1) ]
      # Array.filter movePossible
    where
    movePossible to = case M.lookup (Tuple x y) world, M.lookup to world of
      Just previous, Just this -> cellHeight previous + 1 >= cellHeight this
      _, _ -> false

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

findInHeightMap :: HeightMapCell -> HeightMap -> Maybe (Tuple Int Int)
findInHeightMap target (HeightMap map) = M.toUnfoldable map # Array.filter (eq target <<< snd) # Array.head <#> fst

findHeightMapStart = findInHeightMap Start
findHeightMapEnd = findInHeightMap End

solvePartOne :: HeightMap -> Maybe Int
solvePartOne heightMap = do
  start <- findHeightMapStart heightMap
  { cost } <- findSolutionFrom heightMap start
  Just $ cost - 1

newtype ReverseHeightMap = ReverseHeightMap HeightMap

instance World ReverseHeightMap (Tuple Int Int) where
  lookupCell pos (ReverseHeightMap (HeightMap map)) = M.lookup pos map <#> toDijkstraCell
    where
    toDijkstraCell :: HeightMapCell -> Cell
    toDijkstraCell End = Cell 1
    toDijkstraCell Start = Destination 1
    toDijkstraCell (Height 0) = Destination 1
    toDijkstraCell (Height _) = Cell 1

  adjacentCells pos@(Tuple x y) (ReverseHeightMap (HeightMap world)) =
    [ Tuple (x - 1) y, Tuple (x + 1) y, Tuple x (y - 1), Tuple x (y + 1) ] # Array.filter movePossible
    where
    movePossible to = case M.lookup pos world, M.lookup to world of
      Just previous, Just this -> cellHeight this + 1 >= cellHeight previous
      _, _ -> false

solvePartTwo :: HeightMap -> Maybe Int
solvePartTwo heightMap = do
  peak <- findHeightMapEnd heightMap
  { cost } <- findSolutionFrom (ReverseHeightMap heightMap) peak
  Just $ cost - 1

part :: (HeightMap -> Maybe Int) -> String -> Either Error String
part solve input = parse input <#> solve >>= note (error "No solution found :(") <#> show

partOne = part solvePartOne
partTwo = part solvePartTwo
