module Year2022.Day11 (partOne, partTwo) where

import MeLude

import Control.Monad.Rec.Class (Step(..), tailRecM2)
import Control.Monad.State (State, execState, get, modify_)
import Data.Array as Array
import Data.CodePoint.Unicode (isDecDigit)
import Data.List as List
import Data.Map as M
import Data.String as String
import JS.BigInt (BigInt, fromInt)
import Util (indexed, lines, parseInt, splitStringOnce)

data OperationArgument = OldValue | Number BigInt

data Operation = Multiply OperationArgument | Add OperationArgument

type Actions = { onTrue :: Int, onFalse :: Int }

type Monkey =
  { items :: Array BigInt
  , operation :: Operation
  , test :: BigInt
  , actions :: Actions
  , inspectionCount :: BigInt
  }

parseMonkey :: String -> String |? Monkey
parseMonkey input = case lines input <#> String.trim # Array.filter (not <<< String.null) of
  [ _, items, operation, test, ifTrue, ifFalse ] -> do
    items <- parseItems items
    operation <- parseOperation operation
    test <- parseTest test
    onTrue <- String.toCodePointArray ifTrue # Array.filter isDecDigit # String.fromCodePointArray # parseInt
    onFalse <- String.toCodePointArray ifFalse # Array.filter isDecDigit # String.fromCodePointArray # parseInt
    pure { items, operation, test, actions: { onTrue, onFalse }, inspectionCount: fromInt 0 }
  _ -> Left $ "failed to parse monkey: " <> input

parseItems :: String -> String |? Array BigInt
parseItems line = case splitStringOnce ":" line of
  Nothing -> Left $ "failed to parse items: " <> line
  Just (_ /\ items) ->
    String.split (String.Pattern ",") items
      <#> String.trim
      # Array.filter (not <<< String.null)
      # traverse parseInt
      <#> map fromInt

parseOperation :: String -> String |? Operation
parseOperation line = case splitStringOnce "*" line, splitStringOnce "+" line of
  Nothing, Nothing -> Left $ "failed to parse operation: " <> line
  Just (_ /\ right), _ -> case String.trim right of
    "old" -> Right $ Multiply OldValue
    number -> Multiply <<< Number <$> fromInt <$> parseInt (String.trim number)
  _, (Just (_ /\ right)) -> case String.trim right of
    "old" -> Right $ Add OldValue
    number -> Add <<< Number <$> fromInt <$> parseInt (String.trim number)

parseTest :: String -> String |? BigInt
parseTest = String.toCodePointArray >>> Array.filter isDecDigit >>> String.fromCodePointArray >>> parseInt >>> map fromInt

parse :: String -> String |? Array Monkey
parse = String.replaceAll (String.Pattern "\r") (String.Replacement "")
  >>> String.split (String.Pattern "\n\n")
  >>> traverse parseMonkey

evaluateTest ∷ forall n. Eq n => EuclideanRing n => n → n → Boolean
evaluateTest test n = n `mod` test == zero

modifyMonkeyItems :: (Array BigInt -> Array BigInt) -> Monkey -> Monkey
modifyMonkeyItems f monkey = monkey { items = f $ monkey.items }

addToMonkeyItems :: BigInt -> Monkey -> Monkey
addToMonkeyItems item = modifyMonkeyItems (append (Array.singleton item))

incrementInspectionCount :: Monkey -> Monkey
incrementInspectionCount monkey = monkey { inspectionCount = monkey.inspectionCount + fromInt 1 }

executeOperation :: Operation -> BigInt -> BigInt
executeOperation (Add (Number v)) n = n + v
executeOperation (Multiply (Number v)) n = n * v
executeOperation (Add OldValue) n = n + n
executeOperation (Multiply OldValue) n = n * n

type Monkeys = M.Map Int Monkey

playRound :: (BigInt -> BigInt) -> State Monkeys Unit
playRound f = do
  monkeys <- get
  for_ (Array.range 0 (M.size monkeys - 1)) $ \index -> do
    monkeys <- get
    case M.lookup index monkeys of
      Nothing -> pure unit
      Just monkey -> do
        for_ monkey.items $ executeOperation monkey.operation >>> f >>> \item -> do
          let targetMonkey = if evaluateTest monkey.test item then monkey.actions.onTrue else monkey.actions.onFalse
          modify_ $ M.update (Just <<< addToMonkeyItems item) targetMonkey
          modify_ $ M.update (Just <<< incrementInspectionCount) index
        modify_ $ M.update (Just <<< (modifyMonkeyItems $ const [])) index

repeatState :: forall s a. Int -> State s a -> State s (List a)
repeatState n state = List.reverse <$> tailRecM2 go n List.Nil
  where
  go :: Int -> List a -> State s (Step { a :: Int, b :: List a } (List a))
  go 0 values = pure $ Done values
  go n values = do
    value <- state
    pure $ Loop { a: n - 1, b: value : values }

three = fromInt 3

solvePartOne :: Monkeys -> BigInt
solvePartOne monkeys =
  execState (repeatState 20 (playRound (_ `div` three))) monkeys
    <#> _.inspectionCount
    # M.values
    # List.sort
    # List.reverse
    # List.take 2
    # product

solvePartTwo :: Monkeys -> BigInt
solvePartTwo monkeys =
  execState (repeatState 10000 (playRound (_ `mod` modBase))) monkeys
    <#> _.inspectionCount
    # M.values
    # List.sort
    # List.reverse
    # List.take 2
    # product
  where
  modBase = monkeys <#> _.test # product

partOne :: String -> String |? String
partOne input = parse input <#> indexed <#> M.fromFoldable <#> solvePartOne <#> show

partTwo :: String -> String |? String
partTwo input = parse input <#> indexed <#> M.fromFoldable <#> solvePartTwo <#> show
