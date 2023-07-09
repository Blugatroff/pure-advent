module Year2021.Day8 (day) where

import MeLude

import Data.Array as Array
import Data.List as List
import Data.String as String
import Day (makeDay)
import Util (mapSum)

type Segment = Array Char

parseSegments :: String -> Array Segment
parseSegments = map toCharArray <<< Array.filter (not <<< String.null) <<< map String.trim <$> String.split (String.Pattern " ")

parseLine :: String -> String |? Array Segment /\ Array Segment
parseLine line = case map parseSegments $ String.split (String.Pattern "|") line of
  [ a, b ] -> Right (a /\ b)
  _ -> Left $ "failed to parse line: " <> line

parse :: String -> String |? Array (Array Segment /\ Array Segment)
parse = traverse parseLine
  <<< Array.filter (not <<< String.null)
  <<< map String.trim
  <<< String.split (String.Pattern "\n")

isSuperSet :: forall a. Eq a => Array a -> Array a -> Boolean
isSuperSet a = all (flip Array.elem a)

-- digits with 5 segments: 5 2 3
-- digits with 6 segments: 0 6

-- STEPS
-- - 1: has 2 segments
-- - 7: has 3 segments
-- - 4: has 4 segments
-- - 8: has 7 segments
-- - 3: has 5 segments and contains both segments of 1
-- - 6: has 6 segments and does not contain all segment of 1
-- - 0: has 6 segments and does not contain all segments of the number 4
-- - 9: has 6 segments
-- - 2: has 5 segments and is not a subset of 6
-- - 5: has 5 segments

type Step = Int /\ (Segment -> Array Segment -> Boolean)

steps :: Array Step
steps =
  [ 1 /\ \s _ -> Array.length s == 2
  , 7 /\ \s _ -> Array.length s == 3
  , 4 /\ \s _ -> Array.length s == 4
  , 8 /\ \s _ -> Array.length s == 7
  , 3 /\ \s results -> Array.length s == 5 && isSuperSet s (fromMaybe [] $ Array.index results 1)
  , 6 /\ \s results -> Array.length s == 6 && not (isSuperSet s (fromMaybe [] $ Array.index results 1))
  , 0 /\ \s results -> Array.length s == 6 && not (isSuperSet s (fromMaybe [] $ Array.index results 4))
  , 9 /\ \s _ -> Array.length s == 6
  , 2 /\ \s results -> Array.length s == 5 && not (isSuperSet (fromMaybe [] $ Array.index results 6) s)
  , 5 /\ \s _ -> Array.length s == 5
  ]

solveSegments :: Array Segment -> Maybe (Array Segment)
solveSegments segments = map snd $ Array.foldM f (segments /\ Array.replicate 10 []) steps
  where
  f :: Array Segment /\ Array Segment -> Step -> Maybe (Array Segment /\ Array Segment)
  f (segments /\ results) (n /\ step) = do
    index <- Array.findIndex (flip step results) segments
    pat <- Array.index segments index
    rem <- Array.deleteAt index segments
    new <- Array.updateAt n pat results
    Just $ rem /\ new

matchOutputs :: Array Segment -> Array Segment -> Maybe (Array Int)
matchOutputs segments = sequence
  <<< map ((_ `Array.elemIndex` map Array.sort segments) <<< Array.sort)

digitsToInt :: Array Int -> Maybe Int
digitsToInt = f <<< List.fromFoldable <<< Array.reverse
  where
  f List.Nil = Nothing
  f (n : List.Nil) = Just n
  f (n : list) = f list <#> mul 10 >>> add n

solveLine :: Array Segment /\ Array Segment -> Maybe Int
solveLine (segments /\ outputs) = do
  s <- solveSegments segments
  a <- matchOutputs s outputs
  digitsToInt a

solvePartOne :: Array (Array Segment /\ Array Segment) -> Int
solvePartOne = mapSum (snd >>> mapSum (Array.length >>> isSimpleDigit))
  where
  isSimpleDigit :: Int -> Int
  isSimpleDigit 2 = 1
  isSimpleDigit 4 = 1
  isSimpleDigit 3 = 1
  isSimpleDigit 7 = 1
  isSimpleDigit _ = 0

solvePartTwo :: Array (Array Segment /\ Array Segment) -> Maybe Int
solvePartTwo = sum <<< map solveLine

day = makeDay parse
  (Right <<< solvePartOne >>> show)
  (Right <<< solvePartTwo >>> maybe "No solution found" show)

