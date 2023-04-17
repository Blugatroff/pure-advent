module Year2022.Day18 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.List as List
import Data.Set as S
import Data.String as String
import Util (lines, parseInt)

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

fillAir :: Set Pos -> Set Pos
fillAir cubes = flow (List.singleton { x: minX, y: minY, z: minZ }) mempty
  where
  cubePositions :: Array Pos
  cubePositions = S.toUnfoldable cubes

  xs = _.x <$> cubePositions
  ys = _.y <$> cubePositions
  zs = _.z <$> cubePositions

  minX = (fromMaybe 0 $ minimum xs) - 1
  minY = (fromMaybe 0 $ minimum ys) - 1
  minZ = (fromMaybe 0 $ minimum zs) - 1
  maxX = (fromMaybe 0 $ maximum xs) + 1
  maxY = (fromMaybe 0 $ maximum ys) + 1
  maxZ = (fromMaybe 0 $ maximum zs) + 1

  flow :: List Pos -> Set Pos -> Set Pos
  flow List.Nil visited = visited
  flow ({ x } : rest) visited | x < minX || x > maxX = flow rest visited
  flow ({ y } : rest) visited | y < minY || y > maxY = flow rest visited
  flow ({ z } : rest) visited | z < minZ || z > maxZ = flow rest visited
  flow (pos : rest) visited | S.member pos cubes = flow rest visited
  flow (pos : rest) visited | S.member pos visited = flow rest visited
  flow (pos@{ x, y, z } : rest) visited = S.insert pos visited 
    # flow ({ x: x + 1, y, z } : { x: x + 1, y, z } : { x: x - 1, y, z } : { x, y: y + 1, z } : { x, y: y - 1, z } : { x, y, z: z - 1 } : { x, y, z: z + 1 } : rest)

solvePartTwo ∷ Set Pos → Int
solvePartTwo cubes = Array.length $ Array.filter test $ neighbours =<< S.toUnfoldable cubes
  where
  air = fillAir cubes

  test pos = S.member pos air && (not (S.member pos cubes))

partOne ∷ String → String |? String
partOne input = parse input <#> solvePartOne <#> show

partTwo ∷ String → String |? String
partTwo input = parse input <#> solvePartTwo <#> show
