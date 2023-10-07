module Year2022.Day15 (day) where

import MeLude

import Data.Array as Array
import Data.CodePoint.Unicode (isDecDigit)
import Data.List as List
import Data.Range (Range, (..), tryMergeAll)
import Data.Range as Range
import Data.String as String
import Day (makeDay)
import JS.BigInt (BigInt, fromInt)
import Util (TransparentString(..), lines, parseInt, splitStringOnce)

type Pos = { x :: Int, y :: Int }

type Sensor = { sensor :: Pos, beacon :: Pos }

parseSensor :: String -> String |? Sensor
parseSensor line =
  case splitStringOnce ":" line of
    Nothing -> Left $ "failed to parse sensor " <> line
    Just (sensor /\ beacon) -> do
      sensor <- extractCoords sensor
      beacon <- extractCoords beacon
      pure { sensor, beacon }
  where
  extractCoords input = case splitStringOnce "," input of
    Nothing -> Left $ "failed to parse sensor " <> line
    Just (x /\ y) -> do
      x <- parseInt $ filter x
      y <- parseInt $ filter y
      pure { x, y }

  filter = String.fromCodePointArray <<< Array.filter (\c -> isDecDigit c || codePointFromChar '-' == c) <<< String.toCodePointArray

parse :: String -> String |? (List Sensor)
parse input = lines input
  <#> String.trim
  # Array.filter (not <<< String.null)
  # traverse parseSensor
  <#> List.fromFoldable

manhattan :: Pos -> Pos -> Int
manhattan { x: x1, y: y1 } { x: x2, y: y2 } = abs (x2 - x1) + abs (y2 - y1)

calculateScore :: Pos -> BigInt
calculateScore { x, y } = fromInt x * fromInt 4000000 + fromInt y

rangeFromSensor :: Int -> Sensor -> Maybe (Range Int)
rangeFromSensor y { beacon, sensor } =
  if range < 0 then Nothing
  else Just $ Range.new (sensor.x - range) (sensor.x + range)
  where
  range = manhattan beacon sensor - abs (sensor.y - y)

rangesFromSensors :: Int -> List Sensor -> List (Range Int)
rangesFromSensors y = List.mapMaybe $ rangeFromSensor y

isTestInput :: forall f. Foldable f => f Sensor -> Boolean
isTestInput = all (_.sensor >>> _.x >>> (_ `lessThan` 100000))

part :: forall a. Show a => (Int -> List Sensor -> a) -> Int -> Int -> List Sensor -> String
part solve a b sensors = show $ solve (if isTestInput sensors then a else b) sensors

solvePartOne :: Int -> List Sensor -> Int
solvePartOne ySlice sensors = sum $ (_ - 1) <<< Range.size <$> tryMergeAll (List.fromFoldable ranges)
  where
  ranges = rangesFromSensors ySlice sensors

solvePartTwo :: Int -> List Sensor -> TransparentString
solvePartTwo bounds sensors = TransparentString $ maybe "No solution found" show $ calculateScore <$> f 0
  where
  boundRange = 0 .. bounds

  f y | y > bounds = Nothing
  f y = case ranges y of
    (a : _ : List.Nil) -> Just { y, x: Range.end a + 1 }
    _ -> f (y + 1)

  ranges y = List.mapMaybe (Range.intersection boundRange)
    $ tryMergeAll
    $ rangesFromSensors y sensors

day = makeDay parse
  (Right <<< part solvePartOne 10 2000000)
  (Right <<< part solvePartTwo 10 4000000)

