module Year2021.Day11 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.Foldable (foldr)
import Data.String.CodeUnits as StringCodeUnits
import Util (mapSnd, parseInt, nonEmptyLines)

type Grid = Array (Array Int)

parse = nonEmptyLines >>> traverse (toCharArray >>> traverse (StringCodeUnits.singleton >>> parseInt))

increment :: Grid -> Grid
increment = map (map (add 1))

withCoords :: Grid -> Array (Array (Int /\ Int /\ Int))
withCoords = mapWithIndex (\y -> mapWithIndex (\x cell -> x /\ y /\ cell))

flash :: Grid -> Int -> Int -> Grid
flash grid tx ty = map (map flashCell) (withCoords grid)
  where
  flashCell :: Int /\ Int /\ Int -> Int
  flashCell (x /\ y /\ _) | x == tx && y == ty = 0
  flashCell (_ /\ _ /\ cell) | cell == 0 = cell
  flashCell (x /\ y /\ cell) | (abs (x - tx) <= 1) && (abs (y - ty) <= 1) = cell + 1
  flashCell (_ /\ _ /\ cell) = cell

findFlashing :: Grid -> Array (Int /\ Int)
findFlashing grid =
  Array.concat (withCoords grid)
    # Array.filter (\(_ /\ _ /\ cell) -> cell > 9)
    # map (\(x /\ y /\ _) -> x /\ y)

step :: Grid -> Grid /\ Int
step grid = f $ increment grid
  where
  f :: Grid -> Grid /\ Int
  f grid = if flashed > 0 then mapSnd (add flashed) (f newGrid) else newGrid /\ flashed
    where
    newGrid /\ flashed = foldr fold (grid /\ 0) (findFlashing grid)

  fold :: Int /\ Int -> Grid /\ Int -> Grid /\ Int
  fold (x /\ y) (grid /\ flashed) = flash grid x y /\ (flashed + 1)

solvePartOne :: Grid -> Int
solvePartOne grid = snd $ applyN runStep 100 (grid /\ 0)
  where
  runStep (grid /\ flashes) = step grid # mapSnd (add flashes)

solvePartTwo :: Grid -> Int
solvePartTwo = f 1
  where
  f :: Int -> Grid -> Int
  f n grid = if flashes == height * width then n else f (n + 1) newGrid
    where
    newGrid /\ flashes = step grid
    height = Array.length grid
    width = maybe 0 Array.length $ Array.head grid

partOne input = parse input <#> solvePartOne >>> show
partTwo input = parse input <#> solvePartTwo >>> show
