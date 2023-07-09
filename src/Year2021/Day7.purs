module Year2021.Day7 (day) where

import MeLude

import Data.Array as Array
import Data.List as List
import Data.List.Lazy as LazyList
import Data.String as String
import Day (makeDay)
import Util (parseInt)

parse :: String -> String |? List Int
parse = traverse parseInt <<< List.fromFoldable <<< map String.trim <<< String.split (String.Pattern ",")

distance :: Int -> Int -> Int
distance = map abs <<< sub

moveCost :: Int -> Int -> Int
moveCost a b = sum $ Array.range 0 (distance a b)

fuelCostSum :: forall f. Functor f => Foldable f => (Int -> Int -> Int) -> Int -> f Int -> Int
fuelCostSum costFn pos = sum <<< map (costFn pos)

rangeFrom :: Int -> LazyList.List Int
rangeFrom n = LazyList.List $ defer \_ -> LazyList.Cons n (rangeFrom (n + 1))

costs :: forall f. Functor f => Foldable f => (Int -> Int -> Int) -> f Int -> LazyList.List Int
costs costFn list = map (flip (fuelCostSum costFn) list) $ rangeFrom 0

lastBeforeUp :: LazyList.List Int -> Maybe Int
lastBeforeUp list = case LazyList.uncons list of
  Nothing -> Nothing
  Just { head: a, tail } -> case LazyList.uncons tail of
    Just { head: b, tail } | a >= b -> lastBeforeUp $ LazyList.cons b tail
    _ -> Just a

solvePartOne = fromMaybe 0 <<< lastBeforeUp <<< costs distance
solvePartTwo = fromMaybe 0 <<< lastBeforeUp <<< costs moveCost

day = makeDay parse
  (Right <<< show <<< solvePartOne)
  (Right <<< show <<< solvePartTwo)
