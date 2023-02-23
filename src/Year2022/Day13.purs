module Year2022.Day13 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.CodePoint.Unicode (isDecDigit)
import Data.List as List
import Data.String (codePointFromChar)
import Data.String as String
import Util (indexed, lines, mapFst, pairs, parseInt)

data Element = Number Int | List (List Element)

derive instance Eq Element

instance Ord Element where
  compare (Number a) (Number b) = compare a b
  compare (List a) (List b) = compare a b
  compare a@(Number _) b@(List _) = compare (List (List.singleton a)) b
  compare a@(List _) b@(Number _) = compare a (List (List.singleton b))

parseElement :: List Char -> Either String (Element /\ (List Char))
parseElement List.Nil = Left "expected element got ''"
parseElement input@(c : _) | isDecDigit $ codePointFromChar c = do
  n <- parseInt $ fromCharArray $ Array.fromFoldable $ List.takeWhile (codePointFromChar >>> isDecDigit) input
  Right $ Number n /\ List.dropWhile (codePointFromChar >>> isDecDigit) input
parseElement ('[' : ']' : rest) = Right $ List List.Nil /\ rest
parseElement ('[' : rest) = do
  elems /\ rest <- mapFst List.reverse <$> run List.Nil rest
  case rest of
    ']' : rest -> Right $ List elems /\ rest
    rest -> Left $ "expected ] got '" <> fromCharArray (Array.fromFoldable rest) <> "'"
  where
  run :: List Element -> List Char -> Either String (List Element /\ List Char)
  run previous rest = do
    el /\ rest <- parseElement rest
    case rest of
      ',' : rest -> run (el : previous) rest
      rest -> Right $ (el : previous) /\ rest
parseElement input = Left $ "unexpected input: '" <> fromCharArray (Array.fromFoldable input) <> "'"

parse :: String -> Either String (Array Element)
parse input = lines input <#> String.trim
  # Array.filter (not <<< String.null)
  <#> toCharArray
  >>> List.fromFoldable
  # traverse (map fst <<< parseElement)

solvePartOne :: List Element -> Int
solvePartOne elements = pairs elements <#> (\(a /\ b) -> a < b)
  # indexed
  <#> (mapFst $ add 1)
  # List.filter snd
  <#> fst
  # sum

dividers :: Array Element
dividers =
  [ List $ List.singleton $ List $ List.singleton $ Number 2
  , List $ List.singleton $ List $ List.singleton $ Number 6
  ]

solvePartTwo :: Array Element -> Int
solvePartTwo elements = dividers <#> (\d -> Array.findIndex (eq d) sorted)
  # Array.catMaybes
  <#> add 1
  # product
  where
  sorted = Array.sort (elements <> dividers)

partOne :: String -> Either String String
partOne input = parse input <#> List.fromFoldable >>> solvePartOne >>> show

partTwo :: String -> Either String String
partTwo input = parse input <#> solvePartTwo >>> show
