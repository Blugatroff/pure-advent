module Year2021.Day10 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.List as List
import Data.NonEmpty (NonEmpty, (:|))
import Data.String as String
import Js.BigInt.BigInt (BigInt, fromInt)
import Util (median)

data ParenType = Paren | Bracket | Brace | Triangle
data Paren = Opening ParenType | Closing ParenType

derive instance eqParenType :: Eq ParenType
derive instance eqParen :: Eq Paren

instance showParenType :: Show ParenType where
  show Paren = "Paren"
  show Bracket = "Bracket"
  show Brace = "Brace"
  show Triangle = "Triangle"

instance showParen :: Show Paren where
  show (Opening Paren) = "("
  show (Opening Bracket) = "["
  show (Opening Brace) = "{"
  show (Opening Triangle) = "<"
  show (Closing Paren) = ")"
  show (Closing Bracket) = "]"
  show (Closing Brace) = "}"
  show (Closing Triangle) = ">"

parseParen :: Char -> String |? Paren
parseParen '(' = Right $ Opening Paren
parseParen '[' = Right $ Opening Bracket
parseParen '{' = Right $ Opening Brace
parseParen '<' = Right $ Opening Triangle
parseParen ')' = Right $ Closing Paren
parseParen ']' = Right $ Closing Bracket
parseParen '}' = Right $ Closing Brace
parseParen '>' = Right $ Closing Triangle
parseParen c = Left $ "Unrecognized Paren " <> show c

parse :: String -> String |? Array (Array Paren)
parse =
  String.split (String.Pattern "\n")
    >>> Array.filter (not <<< String.null)
    >>> traverse (toCharArray >>> traverse parseParen)

data Error
  = Incomplete (NonEmpty List ParenType)
  | Invalid ParenType (List ParenType) (List Paren)

showParenTypes direction = showParens <<< List.fromFoldable <<< map direction

instance showError :: Show Error where
  show (Incomplete opening) = "(Incomplete " <> showParenTypes Closing opening <> ")"
  show (Invalid ty stack rest) = "(Invalid " <> show ty <> " " <> showParenTypes Closing stack <> showParens rest <> ")"

match :: (List ParenType /\ List Paren) -> Error |? (List ParenType /\ List Paren)
match (List.Nil /\ List.Nil) = Right $ List.Nil /\ List.Nil
match ((s : stack) /\ List.Nil) = Left $ Incomplete $ s :| stack
match (List.Nil /\ (Opening c : rest)) = match $ (c : List.Nil) /\ rest
match (List.Nil /\ (Closing c : rest)) = Left $ Invalid c List.Nil rest
match ((opening : stack) /\ (Closing c : rest)) | opening == c = match $ stack /\ rest
match (stack /\ (Closing c : rest)) = Left $ Invalid c stack rest
match (stack /\ (Opening c : rest)) = match $ (c : stack) /\ rest

showParens :: List Paren -> String
showParens = map show >>> Array.fromFoldable >>> String.joinWith "" >>> show

lineError :: List Paren -> Maybe Error
lineError line = case match (List.Nil /\ line) of
  Right _ -> Nothing
  Left e -> Just e

solvePartOne :: Array (Array Paren) -> Int
solvePartOne lines = sum $ Array.mapMaybe (errorPoints <=< lineError <<< List.fromFoldable) $ lines
  where
  errorPoints (Incomplete _) = Nothing
  errorPoints (Invalid c _ _) = Just $ parenPoints c

  parenPoints Paren = 3
  parenPoints Bracket = 57
  parenPoints Brace = 1197
  parenPoints Triangle = 25137

solvePartTwo :: Array (Array Paren) -> BigInt
solvePartTwo lines = fromMaybe zero $ median $ Array.mapMaybe (errorPoints <=< lineError <<< List.fromFoldable) $ lines
  where
  errorPoints (Invalid _ _ _) = Nothing
  errorPoints (Incomplete expected) = Just $ linterScore expected

  five = fromInt 5

  linterScore = foldl (\s -> add (five * s) <<< fromInt <<< parenPoints) zero

  parenPoints Paren = 1
  parenPoints Bracket = 2
  parenPoints Brace = 3
  parenPoints Triangle = 4

partOne input = parse input <#> solvePartOne <#> show
partTwo input = parse input <#> solvePartTwo <#> show
