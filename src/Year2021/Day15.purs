module Year2021.Day15 (day) where

import MeLude

import Data.Array as Array
import Data.Pos (Pos(Pos))
import Data.String as String
import Day (makeDay)
import Dijkstra (class World, Cell(..), findPath)
import Util (parseInt)

data Cave = Cave { cells :: Array Int, width :: Int, height :: Int }
instance worldCave :: World Cave Pos Int where
  lookupCell pos@(Pos x y) c@(Cave cave) =
    if isInCave pos c then (if x == cave.width - 1 && y == cave.height - 1 then Destination else Cell) <$> getCell pos c
    else Nothing

  adjacentCells (Pos x y) _ = [ Pos (x - 1) y, Pos (x + 1) y, Pos x (y - 1), Pos x (y + 1) ]

isInCave :: Pos -> Cave -> Boolean
isInCave (Pos x y) (Cave cave) = x >= 0 && y >= 0 && x < cave.width && y < cave.height

getCell :: Pos -> Cave -> Maybe Int
getCell (Pos x y) (Cave cave) = Array.index cave.cells $ (y * cave.width + x)

parse :: String -> String |? Cave
parse input = do
  let lines = String.split (String.Pattern "\n") input # map String.trim # Array.filter (not <<< String.null)
  rows <- traverse (toCharArray >>> traverse (parseInt <<< fromCharArray <<< Array.singleton)) lines
  let cells = Array.concat rows
  let width = fromMaybe 0 $ map Array.length $ Array.head rows
  let height = Array.length rows
  pure (Cave { cells, width, height })

solvePartOne :: Cave -> Maybe Int
solvePartOne cave = findPath cave (Pos 0 0) <#> _.cost

tileCave :: Cave -> Cave
tileCave c@(Cave cave) = Cave { cells, width: cave.width * 5, height: cave.height * 5 }
  where
  cells :: Array Int
  cells = do
    repeatY <- Array.range 0 4
    y <- Array.range 0 (cave.height - 1)
    repeatX <- Array.range 0 4
    x <- Array.range 0 (cave.width - 1)
    pure $ (fromMaybe 0 (getCell (Pos x y) c) + repeatX + repeatY - 1) `mod` 9 + 1

solvePartTwo = tileCave >>> solvePartOne

day = makeDay parse
  (map show <<< note "No Solution found!" <<< solvePartOne)
  (map show <<< note "No Solution found!" <<< solvePartTwo)

