module Year2022.Day21 (partOne, partTwo) where

import MeLude
import Data.Array as Array
import Data.Map as M
import Data.String as String
import JS.BigInt (BigInt, fromInt)
import Util (nonEmptyLines, parseInt, splitStringOnce)

data Operation = Add | Sub | Mul | Div

instance eqOperation :: Eq Operation where
  eq Add Add = true
  eq Sub Sub = true
  eq Mul Mul = true
  eq Div Div = true
  eq _ _ = false

operationOperator Add = "+"
operationOperator Sub = "-"
operationOperator Mul = "*"
operationOperator Div = "/"

runOperation :: forall a. Ring a => Semiring a => EuclideanRing a => Operation -> a -> a -> a
runOperation Add = add
runOperation Sub = sub
runOperation Mul = mul
runOperation Div = div

data Job = Yell Int | Calculate Operation String String

data Monkey = Monkey String Job

parse :: String -> String |? (Map String Monkey)
parse = nonEmptyLines >>> traverse parseMonkey >>> map (map (\m@(Monkey name _) -> name /\ m) >>> M.fromFoldable)

parseMonkey :: String -> String |? Monkey
parseMonkey line = do
  name /\ job <- splitStringOnce ":" line # note ("Failed to parse line: " <> line)
  job <- parseJob job
  Right $ Monkey name job

parseJob :: String -> String |? Job
parseJob job =
  case Array.findMap (\o -> map (o /\ _) $ splitStringOnce (operationOperator o) job) [ Add, Sub, Mul, Div ] of
    Just (o /\ l /\ r) -> Right $ Calculate o (String.trim l) (String.trim r)
    Nothing -> map Yell $ parseInt $ String.trim job

solvePartOne :: Map String Monkey -> Maybe BigInt
solvePartOne monkeys = evaluate =<< calculationFromMonkeys monkeys "root"

data Calculation a = Constant String a | Calculation String Operation (Calculation a) (Calculation a) | Humn

calculationName :: forall a. Calculation a -> Maybe String
calculationName Humn = Nothing
calculationName (Constant name _) = Just name
calculationName (Calculation name _ _ _) = Just name

replace :: forall a. Eq a => (Calculation a -> Boolean) -> Calculation a -> Calculation a -> Calculation a
replace f replacement calc | f calc = replacement
replace f replacement (Calculation name op l r) = Calculation name op (replace f replacement l) (replace f replacement r)
replace _ _ (Constant name value) = Constant name value
replace _ _ Humn = Humn

replaceWithHumn :: forall a. Eq a => String -> Calculation a -> Calculation a
replaceWithHumn name = replace (calculationName >>> map (eq name) >>> fromMaybe false) Humn

instance eqCalculation :: Eq a => Eq (Calculation a) where
  eq Humn Humn = true
  eq (Constant _ l) (Constant _ r) = eq l r
  eq (Calculation _ lop ll lr) (Calculation _ rop rl rr) = eq lop rop && eq ll rl && eq lr rr
  eq _ _ = false

calculationContainsHumn :: forall a. Calculation a -> Boolean
calculationContainsHumn Humn = true
calculationContainsHumn (Constant _ _) = false
calculationContainsHumn (Calculation _ _ l r) = calculationContainsHumn l || calculationContainsHumn r

calculationFromMonkeys :: Map String Monkey -> String -> Maybe (Calculation BigInt)
calculationFromMonkeys monkeys name = do
  Monkey _ job <- M.lookup name monkeys
  case job of
    Yell n -> Just $ Constant name $ fromInt n
    Calculate op l r -> do
      l <- calculationFromMonkeys monkeys l
      r <- calculationFromMonkeys monkeys r
      Just $ Calculation name op l r

evaluate :: forall a. Ring a => Semiring a => EuclideanRing a => Calculation a -> Maybe a
evaluate (Constant _ n) = Just n
evaluate (Calculation _ op l r) = evaluate l >>= \l -> evaluate r <#> runOperation op l
evaluate Humn = Nothing

peelCalculation :: forall a. Show a => Ring a => Calculation a -> Calculation a -> Calculation a
peelCalculation (Calculation name Add l r) | calculationContainsHumn l = peelCalculation l <<< \rhs -> Calculation name Sub rhs r
peelCalculation (Calculation name Add l r) | calculationContainsHumn r = peelCalculation r <<< \rhs -> Calculation name Sub rhs l
peelCalculation (Calculation name Sub l r) | calculationContainsHumn l = peelCalculation l <<< Calculation name Add r
peelCalculation (Calculation name Sub l r) | calculationContainsHumn r = peelCalculation r <<< Calculation name Sub l
peelCalculation (Calculation name Div l r) | calculationContainsHumn l = peelCalculation l <<< Calculation name Mul r
peelCalculation (Calculation name Div l r) | calculationContainsHumn r = peelCalculation r <<< Calculation name Div l
peelCalculation (Calculation name Mul l r) | calculationContainsHumn l = peelCalculation l <<< \rhs -> Calculation name Div rhs r
peelCalculation (Calculation name Mul l r) | calculationContainsHumn r = peelCalculation r <<< \rhs -> Calculation name Div rhs l
peelCalculation (Calculation _ _ _ _) = identity
peelCalculation (Constant _ _) = identity
peelCalculation Humn = identity

solvePartTwo :: Map String Monkey -> Maybe BigInt
solvePartTwo monkeys = do
  Monkey _ job <- M.lookup "root" monkeys
  _ /\ l /\ r <- case job of
    Yell _ -> Nothing
    Calculate op l r -> Just $ op /\ l /\ r
  lc <- replaceWithHumn "humn" <$> calculationFromMonkeys monkeys l
  rc <- replaceWithHumn "humn" <$> calculationFromMonkeys monkeys r
  humn /\ constant <- case calculationContainsHumn lc, calculationContainsHumn rc of
    true, true -> Nothing
    false, false -> Nothing
    true, false -> Just $ lc /\ rc
    false, true -> Just $ rc /\ lc
  evaluate $ peelCalculation humn constant

solve f = parse >>> map (f >>> map show >>> fromMaybe "No solution found!")

partOne = solve solvePartOne
partTwo = solve solvePartTwo

