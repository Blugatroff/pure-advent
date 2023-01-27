module Year2022.Day18 (partOne, partTwo) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as S
import Data.String as String
import Data.Traversable (maximum, minimum, traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import Util (lines, parseInt)

type Pos = { x :: Int, y :: Int, z :: Int }

parseLine :: String -> Either Error Pos
parseLine s = String.split (String.Pattern ",") s
  <#> String.trim
  # traverse parseInt
  >>= case _ of
    [ x, y, z ] -> Right { x, y, z }
    _ -> Left $ error $ "failed to parse "

parse :: String -> Either Error (Set Pos)
parse input = lines input
  <#> String.trim
  # Array.filter (not <<< String.null)
  # traverse parseLine
  <#> S.fromFoldable

neighbours { x, y, z } =
  [ { x: x - 1, y, z }
  , { x: x + 1, y, z }
  , { x, y: y - 1, z }
  , { x, y: y + 1, z }
  , { x, y, z: z - 1 }
  , { x, y, z: z + 1 }
  ]

solvePartOne cubes =
  cubes
    # S.toUnfoldable
    >>= neighbours
    # Array.filter (\p -> not $ S.member p cubes)
    # Array.length

fill cubes from = flow from S.empty
  where
  flow { x } _ | x < minX || x > maxX = Nothing
  flow { y } _ | y < minY || y > maxY = Nothing
  flow { z } _ | z < minZ || z > maxZ = Nothing
  flow pos visited | S.member pos cubes = Just visited
  flow pos visited | S.member pos visited = Just visited
  flow (pos@{ x, y, z }) visited =
    flow { x: x + 1, y, z } (S.insert pos visited)
      >>= flow { x: x + 1, y, z }
      >>= flow { x: x - 1, y, z }
      >>= flow { x, y: y + 1, z }
      >>= flow { x, y: y - 1, z }
      >>= flow { x, y, z: z - 1 }
      >>= flow { x, y, z: z + 1 }

  cubePositions :: Array Pos
  cubePositions = S.toUnfoldable cubes

  minX = cubePositions <#> _.x # minimum # fromMaybe 0
  minY = cubePositions <#> _.y # minimum # fromMaybe 0
  minZ = cubePositions <#> _.z # minimum # fromMaybe 0
  maxX = cubePositions <#> _.x # maximum # fromMaybe 0
  maxY = cubePositions <#> _.y # maximum # fromMaybe 0
  maxZ = cubePositions <#> _.z # maximum # fromMaybe 0

solvePartTwo cubes = Array.length $ Array.filter (flip M.lookup lookup >>> fromMaybe false) neighs
  where
  neighs :: Array Pos
  neighs = S.toUnfoldable cubes >>= neighbours

  distinctNeighbours :: Array Pos
  distinctNeighbours = S.toUnfoldable $ S.fromFoldable neighs

  lookup :: M.Map Pos Boolean
  lookup = distinctNeighbours
    # map (\pos -> Tuple pos (fill cubes pos == Nothing))
    # M.fromFoldable

partOne input = parse input <#> solvePartOne <#> show
partTwo input = parse input <#> solvePartTwo <#> show
