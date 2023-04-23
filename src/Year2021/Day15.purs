module Year2021.Day15 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.String as String
import Dijkstra (class World, Cell(..), findSolutionFrom)
import Util (parseInt)

data Cave = Cave { cells :: Array Int, width :: Int, height :: Int }

instance worldCave :: World Cave (Int /\ Int) where
  lookupCell pos@(x /\ y) c@(Cave cave) =
    if isInCave pos c then (if x == cave.width - 1 && y == cave.height - 1 then Destination else Cell) <$> getCell pos c
    else Nothing

  adjacentCells (x /\ y) _ = [ (x - 1) /\ y, (x + 1) /\ y, x /\ (y - 1), x /\ (y + 1) ]

isInCave :: (Int /\ Int) -> Cave -> Boolean
isInCave (x /\ y) (Cave cave) = x >= 0 && y >= 0 && x < cave.width && y < cave.height

getCell :: (Int /\ Int) -> Cave -> Maybe Int
getCell (x /\ y) (Cave cave) = Array.index cave.cells $ (y * cave.width + x)

parse :: String -> String |? Cave
parse input = do
  let lines = String.split (String.Pattern "\n") input # map String.trim # Array.filter (not <<< String.null)
  rows <- traverse (toCharArray >>> traverse (parseInt <<< fromCharArray <<< Array.singleton)) lines
  let cells = Array.concat rows
  let width = fromMaybe 0 $ map Array.length $ Array.head rows
  let height = Array.length rows
  pure (Cave { cells, width, height })

solvePartOne :: Cave -> Maybe Int
solvePartOne cave = findSolutionFrom cave (0 /\ 0) <#> _.cost <#> sub (fromMaybe 0 $ getCell (0 /\ 0) cave) <#> negate

tileCave :: Cave -> Cave
tileCave c@(Cave cave) = Cave { cells, width: cave.width * 5, height: cave.height * 5 }
  where
  cells :: Array Int
  cells = do
    repeatY <- Array.range 0 4
    y <- Array.range 0 (cave.height - 1)
    repeatX <- Array.range 0 4
    x <- Array.range 0 (cave.width - 1)
    pure $ (fromMaybe 0 (getCell (x /\ y) c) + repeatX + repeatY - 1) `mod` 9 + 1

solvePartTwo = tileCave >>> solvePartOne

partOne = parse >>> map solvePartOne >>> map (maybe "No Solution found!" show)
partTwo = parse >>> map solvePartTwo >>> map (maybe "No Solution found!" show)

