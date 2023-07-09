module Year2021.Day9 (day) where

import MeLude

import Data.Array as Array
import Data.Map as M
import Data.String as String
import Day (makeDay)
import Util (indexed, parseInt)

type HeightMap = Array (Array Int)

parse :: String -> String |? HeightMap
parse =
  String.split (String.Pattern "\n")
    >>> Array.filter (not <<< String.null)
    >>> traverse (toCharArray >>> traverse (\c -> parseInt (fromCharArray [ c ])))

sampleHeightMap :: Int -> Int -> HeightMap -> Maybe Int
sampleHeightMap x y rows = Array.index rows y >>= flip Array.index x

isLowPoint :: HeightMap -> Int -> Int -> Maybe Boolean
isLowPoint grid x y = sampleHeightMap x y grid <#> test
  where
  test h = test x (y - 1) && test (x - 1) y && test x (y + 1) && test (x + 1) y
    where
    test x y = maybe true (_ `greaterThan` h) $ sampleHeightMap x y grid

solvePartOne :: HeightMap -> Int
solvePartOne grid = sum do
  y /\ row <- indexed grid
  x /\ h <- indexed row
  case isLowPoint grid x y of
    Nothing -> []
    Just true -> pure (h + 1)
    Just false -> []

flow :: HeightMap -> Int -> Int -> Maybe (Int /\ Int)
flow grid x y | isLowPoint grid x y == Just true = Just $ x /\ y
flow grid x y | sampleHeightMap (x - 1) y grid < sampleHeightMap x y grid = flow grid (x - 1) y
flow grid x y | sampleHeightMap x (y - 1) grid < sampleHeightMap x y grid = flow grid x (y - 1)
flow grid x y | sampleHeightMap (x + 1) y grid < sampleHeightMap x y grid = flow grid (x + 1) y
flow grid x y | sampleHeightMap x (y + 1) grid < sampleHeightMap x y grid = flow grid x (y + 1)
flow _ _ _ = Nothing

solvePartTwo grid =
  product $ Array.take 3 $ Array.reverse $ Array.sort $ map snd $ M.toUnfoldable $ foldl f M.empty do
    y /\ row <- indexed grid
    x /\ h <- indexed row
    if h /= 9 then [ flow grid x y ] else []
  where
  f :: Map (Int /\ Int) Int -> Maybe (Int /\ Int) -> Map (Int /\ Int) Int
  f m Nothing = m
  f m (Just (x /\ y)) = M.alter (\c -> Just $ fromMaybe 0 c + 1) (x /\ y) m

day = makeDay parse
  (Right <<< show <<< solvePartOne)
  (Right <<< show <<< solvePartTwo)

