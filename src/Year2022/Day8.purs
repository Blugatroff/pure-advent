module Year2022.Day8 (day) where

import MeLude

import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SC
import Day (makeDay)
import Util (parseInt)

type Grid = Array (Array Int)

indexGrid :: Int /\ Int -> Grid -> Maybe Int
indexGrid (x /\ y) grid = Array.index grid y >>= flip Array.index x

parse :: String -> String |? Grid
parse input = String.split (Pattern "\n") input
  <#> String.trim
  # Array.filter (not <<< String.null)
  # traverse (SC.toCharArray >>> traverse (SC.singleton >>> parseInt))

pathsToBorder :: Int /\ Int -> Int /\ Int -> Array (Array (Int /\ Int))
pathsToBorder (width /\ height) (x /\ y) = [ top, bottom, left, right ]
  where
  top = Array.range (y - 1) 0 <#> (/\) x
  bottom = Array.range (y + 1) (height - 1) <#> (/\) x
  left = Array.range (x - 1) 0 <#> (_ /\ y)
  right = Array.range (x + 1) (width - 1) <#> (_ /\ y)

isVisible :: Grid -> Int /\ Int -> Boolean
isVisible grid pos = pathsToBorder (gridSize grid) pos # any (all isLower)
  where
  h = fromMaybe 0 $ indexGrid pos grid

  isLower :: Int /\ Int -> Boolean
  isLower p = indexGrid p grid # fromMaybe 0 # (_ `lessThan` h)

scenicScore :: Grid -> Int /\ Int -> Int
scenicScore grid pos = pathsToBorder (gridSize grid) pos <#> viewDistance # product
  where
  h = fromMaybe 0 $ indexGrid pos grid

  isLower p = indexGrid p grid # fromMaybe 0 # (_ `lessThan` h)

  viewDistance list = if numberSmallerTrees == Array.length list then numberSmallerTrees else numberSmallerTrees + 1
    where
    numberSmallerTrees = Array.length $ Array.takeWhile isLower list

gridWidth :: Grid -> Int
gridWidth grid = Array.index grid 0 # maybe 0 Array.length

gridHeight :: Grid -> Int
gridHeight = Array.length

gridSize :: Grid -> Int /\ Int
gridSize grid = gridWidth grid /\ gridHeight grid

gridPositions :: Grid -> Array (Int /\ Int)
gridPositions grid = Array.range 0 (gridHeight grid - 1) >>= (\y -> Array.range 0 (gridWidth grid - 1) <#> (/\) y)

solvePartOne :: Grid -> Int
solvePartOne grid = gridPositions grid # Array.filter (isVisible grid) # Array.length

solvePartTwo :: Grid -> Int
solvePartTwo grid = gridPositions grid <#> scenicScore grid # maximum # fromMaybe 0

day = makeDay parse
  (Right <<< show <<< solvePartOne)
  (Right <<< show <<< solvePartTwo)

