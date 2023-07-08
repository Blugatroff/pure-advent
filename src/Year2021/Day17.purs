module Year2021.Day17 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.List as List
import Data.String as String
import Util (parseInt, sign, splitStringOnce)

type Target = { minX :: Int, maxX :: Int, minY :: Int, maxY :: Int }

parseRange :: String -> String |? (Int /\ Int)
parseRange range = case splitStringOnce ".." (String.drop 1 $ String.dropWhile (notEq $ codePointFromChar '=') range) of
  Nothing -> Left $ "failed to parse range: " <> range
  Just (min /\ max) -> do
    min <- lmap (const $ "failed to parse min: " <> min <> " of range: " <> range) $ parseInt min
    max <- lmap (const $ "failed to parse max: " <> max <> " of range: " <> range) $ parseInt max
    pure $ min /\ max

parse :: String -> String |? Target
parse input = do
  ranges <- splitStringOnce ":" input <#> snd <#> String.trim # note ("failed to parse line: " <> input)
  (xrange /\ yrange) <- note ("failed to parse line: " <> input) $ splitStringOnce "," ranges
  (minX /\ maxX) <- parseRange xrange
  (minY /\ maxY) <- parseRange yrange
  Right { minX, maxX, minY, maxY }

type Point = Int /\ Int

type Velocity = Int /\ Int

drag :: Int -> Int
drag n = n - sign n

step :: Point /\ Velocity -> Point /\ Velocity
step ((x /\ y) /\ (vx /\ vy)) = ((x + vx) /\ (y + vy)) /\ ((drag vx) /\ (vy - 1))

isInTarget :: Target -> (Int /\ Int) -> Boolean
isInTarget target (x /\ y) = x >= target.minX && x <= target.maxX && y >= target.minY && y <= target.maxY

targetReachable :: Target -> Point /\ Velocity -> Boolean
targetReachable target ((_ /\ y) /\ (_ /\ vy)) = not $ vy < 0 && y < target.minY

type Shot = List (Point /\ Velocity)

shoot :: Target -> (Point /\ Velocity) -> Maybe Shot
shoot target (point /\ velocity)
  | not $ targetReachable target (point /\ velocity) = Nothing
  | isInTarget target point = Just $ List.singleton (point /\ velocity)
  | otherwise = step (point /\ velocity) # shoot target <#> ((point /\ velocity) : _)

shotScore :: Shot -> Int
shotScore steps = steps <#> fst <#> snd # maximum # fromMaybe 0

findBestShot :: forall f. Functor f => Foldable f => f Shot -> Maybe (Int /\ Shot)
findBestShot steps = steps <#> (\shot -> (shotScore shot) /\ shot) # maximumBy (compare `on` fst)

scanRange :: Target -> Array Shot
scanRange target = do
  vx <- xrange 
  Array.range target.minY 2000 
    # Array.mapMaybe \vy -> shoot target ((0 /\ 0) /\ (vx /\ vy))
  where
    xrange
      | target.minX > 0 = Array.range 0 target.maxX
      | target.maxX < 0 = Array.range 0 (abs target.minX) <#> negate
      | otherwise = Array.range target.minX target.maxX

solvePartOne :: Target -> Int
solvePartOne = maybe 0 fst <<< findBestShot <<< scanRange

solvePartTwo :: Target -> Int
solvePartTwo = Array.length <<< scanRange

partOne :: String -> String |? String
partOne = parse >>> map (solvePartOne >>> show)

partTwo :: String -> String |? String
partTwo = parse >>> map (solvePartTwo >>> show)
