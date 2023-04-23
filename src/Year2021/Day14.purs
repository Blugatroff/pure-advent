module Year2021.Day14 (partOne, partTwo) where

import MeLude

import Control.Monad.State (State, modify_, get, execState)
import Data.Array as Array
import Data.Foldable (fold, maximumBy)
import Data.Function (applyN)
import Data.List as List
import Data.Map as M
import Data.String as String
import Js.BigInt.BigInt (BigInt, fromInt)
import Util (dedupCount, mapFst, mapSnd, toCharUnfoldable, windows, windows2)

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

findRule :: List Char -> List Rule -> Maybe Rule
findRule (left : right : List.Nil) = List.find (\(Rule l r _) -> l == left && r == right)
findRule _ = const Nothing

applyRules :: List Rule -> List Char -> List Char
applyRules rules template = foldl f List.Nil $ windows 2 template
  where
  f :: List Char -> List Char -> List Char
  f out window =
    out <> case findRule window rules of
      Just (Rule _ _ between) -> List.take 1 window <> List.singleton between
      Nothing -> List.take 1 window

computeScore :: List Char -> String |? Int
computeScore template = do
  let error = "cannot compute score of empty template"
  let ddc = dedupCount :: _ -> Array _
  let ddcs = ddc template
  mostCommon <- ddcs # maximumBy (compare `on` snd) # note error <#> snd
  leastCommon <- ddcs # minimumBy (compare `on` snd) # note error <#> snd
  pure $ mostCommon - leastCommon

solvePartOne :: Int -> Input -> String |? Int
solvePartOne n input = applyN (applyRules input.rules) n input.template # computeScore

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

applyRulesPartTwo :: List Rule -> State (Pairs /\ Letters) Unit
applyRulesPartTwo rules = do
  oldPairs <- get <#> fst
  for_ rules \rule -> do
    applyRule rule oldPairs

computeScorePartTwo :: Letters -> BigInt
computeScorePartTwo letters = last - first
  where
  list = Array.sort $ map snd $ M.toUnfoldable letters
  first = fromMaybe zero $ Array.head list
  last = fromMaybe zero $ Array.last list

runSteps :: Int -> List Rule -> State (Pairs /\ Letters) Unit
runSteps n rules = for_ (Array.range 1 n) $ \_ -> applyRulesPartTwo rules

solvePartTwo :: Int -> Input -> BigInt
solvePartTwo n input =
  execState (runSteps n input.rules) initialState
    # snd
    # computeScorePartTwo
  where
  initialState = (getPairs input.template) /\ map fromInt (M.fromFoldable (dedupCount input.template :: Array _))

partOne input = parse input >>= solvePartOne 10 >>> map show
partTwo input = parse input <#> solvePartTwo 40 >>> show
