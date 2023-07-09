module Year2021.Day14 (day) where

import MeLude

import Control.Monad.State (State, modify_, get, execState)
import Data.Array as Array
import Data.List as List
import Data.Map as M
import Data.String as String
import Day (makeDay)
import JS.BigInt (BigInt, fromInt)
import Util (dedupCount, mapFst, mapSnd, toCharUnfoldable, windows2, repeatM)

data Rule = Rule Char Char Char

type Input = { template :: List Char, rules :: List Rule }

parseRule :: String -> String |? Rule
parseRule line = case String.split (String.Pattern "->") line # map String.trim # fold # toCharArray of
  [ left, right, between ] -> Right $ Rule left right between
  _ -> Left $ "failed to parse line: " <> line

parse :: String -> String |? Input
parse input =
  case String.split (String.Pattern "\n") input # map String.trim # Array.filter (not <<< String.null) # List.fromFoldable of
    (template : rules) -> traverse parseRule rules <#> (\rules -> { template: toCharUnfoldable template, rules })
    _ -> Left $ "failed to parse: " <> input

type Pair = Char /\ Char
type Pairs = M.Map Pair BigInt
type Letters = M.Map Char BigInt

getPairs :: List Char -> Pairs
getPairs = windows2 <#> (dedupCount :: _ -> Array _) >>> map (mapSnd fromInt) >>> M.fromFoldable

applyRule :: Rule -> Pairs -> State (Pairs /\ Letters) Unit
applyRule (Rule left right between) pairs = case M.lookup key pairs of
  Nothing -> pure unit
  Just count | count == zero -> pure unit
  Just count -> do
    modify_ $ mapFst $ M.alter (Just <<< (add (-count)) <<< fromMaybe zero) key
    modify_ $ mapFst $ M.alter (Just <<< (add count) <<< fromMaybe zero) leftSide
    modify_ $ mapFst $ M.alter (Just <<< (add count) <<< fromMaybe zero) rightSide
    modify_ $ mapSnd $ M.alter (Just <<< (add count) <<< fromMaybe zero) between

  where
  key = left /\ right
  leftSide = left /\ between
  rightSide = between /\ right

applyRules :: List Rule -> State (Pairs /\ Letters) Unit
applyRules rules = do
  oldPairs <- get <#> fst
  for_ rules \rule -> do
    applyRule rule oldPairs

computeScore :: Letters -> BigInt
computeScore letters = last - first
  where
  list = Array.sort $ map snd $ M.toUnfoldable letters
  first = fromMaybe zero $ Array.head list
  last = fromMaybe zero $ Array.last list

runSteps :: Int -> List Rule -> State (Pairs /\ Letters) Unit
runSteps n rules = repeatM n $ applyRules rules

solve :: Int -> Input -> BigInt
solve n input =
  execState (runSteps n input.rules) initialState
    # snd
    # computeScore
  where
  initialState = (getPairs input.template) /\ map fromInt (M.fromFoldable (dedupCount input.template :: Array _))

day = makeDay parse
  (Right <<< show <<< solve 10)
  (Right <<< show <<< solve 40)

