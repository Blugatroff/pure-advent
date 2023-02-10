module Year2022.Day15 (partOne, partTwo) where

import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either(..))
import Data.Function (on)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (abs, lessThan)
import Data.String (codePointFromChar)
import Data.String as String
import Data.Traversable (class Foldable, all, sum, traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import Js.BigInt.BigInt (BigInt, fromInt)
import Util (TransparentString(..), lines, parseInt, splitStringOnce)

type Pos = { x :: Int, y :: Int }

type Sensor = { sensor :: Pos, beacon :: Pos }

parseSensor :: String -> Either Error Sensor
parseSensor line =
  case splitStringOnce ":" line of
    Nothing -> Left $ error $ "failed to parse sensor " <> line
    Just (Tuple sensor beacon) -> do
      sensor <- extractCoords sensor
      beacon <- extractCoords beacon
      pure { sensor, beacon }
  where
  extractCoords input = case splitStringOnce "," input of
    Nothing -> Left $ error $ "failed to parse sensor " <> line
    Just (Tuple x y) -> do
      x <- parseInt $ filter x
      y <- parseInt $ filter y
      pure { x, y }

  filter = String.fromCodePointArray <<< Array.filter (\c -> isDecDigit c || codePointFromChar '-' == c) <<< String.toCodePointArray

parse :: String -> Either Error (List Sensor)
parse input = lines input
  <#> String.trim
  # Array.filter (not <<< String.null)
  # traverse parseSensor
  <#> List.fromFoldable

manhattan :: Pos -> Pos -> Int
manhattan { x: x1, y: y1 } { x: x2, y: y2 } = abs (x2 - x1) + abs (y2 - y1)

calculateScore :: Pos -> BigInt
calculateScore { x, y } = fromInt x * fromInt 4000000 + fromInt y

data Range = Range Int Int

newRange :: Int -> Int -> Range
newRange s e | e >= s = Range s e
newRange s e = Range e s

rangeStart :: Range -> Int
rangeStart (Range s _) = s

rangeEnd :: Range -> Int
rangeEnd (Range _ e) = e

inRange :: Range -> Int -> Boolean
inRange (Range s e) n = n >= s && n <= e

rangeSize :: Range -> Int
rangeSize (Range s e) = abs $ e - s

intersects :: Range -> Range -> Boolean
intersects l@(Range sl el) r@(Range sr er) = inRange r sl || inRange r el || inRange l sr || inRange l er

tryMerge :: Range -> Range -> Either (Tuple Range Range) Range
tryMerge l@(Range sl el) (Range sr er) | inRange l sr = Right $ Range sl (max el er)
tryMerge l@(Range sl el) (Range sr er) | inRange l er = Right $ Range (min sl sr) el
tryMerge (Range sl el) r@(Range sr er) | inRange r sl = Right $ Range sr (max el er)
tryMerge (Range sl el) r@(Range sr er) | inRange r el = Right $ Range (min sl sr) er
tryMerge (Range sl el) (Range sr er) | el + 1 == sr = Right $ Range sl er
tryMerge (Range sl el) (Range sr er) | er + 1 == sl = Right $ Range sr el
tryMerge l r = Left $ Tuple l r

tryMergeAll :: List Range -> List Range
tryMergeAll ranges = f $ List.sortBy (compare `on` rangeStart) ranges
  where
  f :: List Range -> List Range
  f List.Nil = List.Nil
  f (r : List.Nil) = List.singleton r
  f (a : b : rest) = case tryMerge a b of
    Left (Tuple a b) -> a : f (b : rest)
    Right r -> f $ r : rest

andRange :: Range -> Range -> Maybe Range
andRange a@(Range as ae) b@(Range bs be) | intersects a b = Just $ Range (max as bs) (min ae be)
andRange _ _ = Nothing

rangeFromSensor :: Int -> Sensor -> Maybe Range
rangeFromSensor y { beacon, sensor } =
  if range < 0 then Nothing
  else Just $ newRange (sensor.x - range) (sensor.x + range)
  where
  range = manhattan beacon sensor - abs (sensor.y - y)

rangesFromSensors :: Int -> List Sensor -> List Range
rangesFromSensors y = List.mapMaybe $ rangeFromSensor y

isTestInput :: forall f. Foldable f => f Sensor -> Boolean
isTestInput = all (_.sensor >>> _.x >>> (_ `lessThan` 100000))

part :: forall a. Show a => (Int -> List Sensor -> a) -> Int -> Int -> String -> Either Error String
part solve a b input = parse input
  <#> \sensors -> show $ solve (if isTestInput sensors then a else b) sensors

solvePartOne :: Int -> List Sensor -> Int
solvePartOne ySlice sensors = sum $ rangeSize <$> tryMergeAll (List.fromFoldable ranges)
  where
  ranges = rangesFromSensors ySlice sensors

solvePartTwo :: Int -> List Sensor -> TransparentString
solvePartTwo bounds sensors = TransparentString $ maybe "No solution found" show $ calculateScore <$> f 0
  where
  boundRange = Range 0 bounds

  f y | y > bounds = Nothing
  f y = case ranges y of
    (a : _ : List.Nil) -> Just { y, x: rangeEnd a + 1 }
    _ -> f (y + 1)

  ranges y = List.mapMaybe (andRange boundRange)
    $ tryMergeAll
    $ rangesFromSensors y sensors

partOne = part solvePartOne 10 2000000
partTwo = part solvePartTwo 20 4000000

