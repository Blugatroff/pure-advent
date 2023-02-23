module Year2022.Day18 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.List as List
import Data.Map as M
import Data.Set as S
import Data.String as String
import Data.Tuple (Tuple(..))
import Util (bindMaybes, lines, parseInt)

type Pos = { x :: Int, y :: Int, z :: Int }

parseLine :: String -> String |? Pos
parseLine s = String.split (String.Pattern ",") s
  <#> String.trim
  # traverse parseInt
  >>= case _ of
    [ x, y, z ] -> Right { x, y, z }
    _ -> Left "failed to parse "

parse :: String -> String |? (Set Pos)
parse input = lines input
  <#> String.trim
  # Array.filter (not <<< String.null)
  # traverse parseLine
  <#> S.fromFoldable

neighbours ∷ Pos → Array Pos
neighbours { x, y, z } =
  [ { x: x - 1, y, z }
  , { x: x + 1, y, z }
  , { x, y: y - 1, z }
  , { x, y: y + 1, z }
  , { x, y, z: z - 1 }
  , { x, y, z: z + 1 }
  ]

solvePartOne ∷ Set Pos → Int
solvePartOne cubes =
  cubes
    # S.toUnfoldable
    >>= neighbours
    # Array.filter (\p -> not $ S.member p cubes)
    # Array.length

fill ∷ Set Pos → Pos → Maybe (Set Pos)
fill cubes from = flow from S.empty
  where
  flow :: Pos -> Set Pos -> Maybe (Set Pos)
  flow { x } _ | x < minX || x > maxX = Nothing
  flow { y } _ | y < minY || y > maxY = Nothing
  flow { z } _ | z < minZ || z > maxZ = Nothing
  flow pos visited | S.member pos cubes = Just visited
  flow pos visited | S.member pos visited = Just visited
  flow (pos@{ x, y, z }) visited =
    flip bindMaybes (S.insert pos visited) $ List.fromFoldable
      [ flow { x: x + 1, y, z }
      , flow { x: x + 1, y, z }
      , flow { x: x - 1, y, z }
      , flow { x, y: y + 1, z }
      , flow { x, y: y - 1, z }
      , flow { x, y, z: z - 1 }
      , flow { x, y, z: z + 1 }
      ]

  cubePositions :: Array Pos
  cubePositions = S.toUnfoldable cubes

  xs = _.x <$> cubePositions
  ys = _.y <$> cubePositions
  zs = _.z <$> cubePositions

  minX = fromMaybe 0 $ minimum xs
  minY = fromMaybe 0 $ minimum ys
  minZ = fromMaybe 0 $ minimum zs
  maxX = fromMaybe 0 $ maximum xs
  maxY = fromMaybe 0 $ maximum ys
  maxZ = fromMaybe 0 $ maximum zs

solvePartTwo ∷ Set Pos → Int
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

partOne ∷ String → String |? String
partOne input = parse input <#> solvePartOne <#> show

partTwo ∷ String → String |? String
partTwo input = parse input <#> solvePartTwo <#> show