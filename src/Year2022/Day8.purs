module Year2022.Day8 (partOne, partTwo) where

import Prelude

import Data.Array (filter, index, length, range, takeWhile)
import Data.Either (Either)
import Data.Foldable (all, maximum, product)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Ord (lessThan)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SC
import Data.Traversable (any, traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import Util (parseInt)

type Grid = Array (Array Int)

indexGrid :: Tuple Int Int -> Grid -> Maybe Int
indexGrid (Tuple x y) grid = index grid y >>= flip index x

parse :: String -> Either Error Grid
parse input = String.split (Pattern "\n") input
  <#> String.trim
  # filter (not <<< String.null)
  # traverse (SC.toCharArray >>> traverse (SC.singleton >>> parseInt))

pathsToBorder :: Tuple Int Int -> Tuple Int Int -> Array (Array (Tuple Int Int))
pathsToBorder (Tuple width height) (Tuple x y) = [top, bottom, left, right]
  where
    top = range (y - 1) 0 <#> Tuple x
    bottom = range (y + 1) (height - 1) <#> Tuple x
    left = range (x - 1) 0 <#> (_ `Tuple` y)
    right = range (x + 1) (width - 1) <#> (_ `Tuple` y)

isVisible :: Grid -> Tuple Int Int -> Boolean
isVisible grid pos = pathsToBorder (gridSize grid) pos # any (all isLower)
  where
    h = fromMaybe 0 $ indexGrid pos grid

    isLower :: Tuple Int Int -> Boolean
    isLower p = indexGrid p grid # fromMaybe 0 # (_ `lessThan` h)

scenicScore :: Grid -> Tuple Int Int -> Int
scenicScore grid pos = pathsToBorder (gridSize grid) pos <#> viewDistance # product
  where
    h = fromMaybe 0 $ indexGrid pos grid

    isLower p = indexGrid p grid # fromMaybe 0 # (_ `lessThan` h)

    viewDistance list = if numberSmallerTrees == length list then numberSmallerTrees else numberSmallerTrees + 1
      where
        numberSmallerTrees = length $ takeWhile isLower list

gridWidth :: Grid -> Int
gridWidth grid = index grid 0 # maybe 0 length

gridHeight :: Grid -> Int
gridHeight = length

gridSize :: Grid -> Tuple Int Int
gridSize grid = Tuple (gridWidth grid) (gridHeight grid)

gridPositions :: Grid -> Array (Tuple Int Int)
gridPositions grid = range 0 (gridHeight grid - 1) >>= (\y -> range 0 (gridWidth grid - 1) <#> Tuple y)

solvePartOne :: Grid -> Int
solvePartOne grid = gridPositions grid # filter (isVisible grid) # length

solvePartTwo :: Grid -> Int
solvePartTwo grid = gridPositions grid <#> scenicScore grid # maximum # fromMaybe 0

partOne :: String -> Either Error String
partOne input = parse input <#> solvePartOne <#> show

partTwo :: String -> Either Error String
partTwo input = parse input <#> solvePartTwo <#> show
