module Year2022.Day13 (partOne, partTwo) where

import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either(..))
import Data.Foldable (product, sum)
import Data.List (List, (:))
import Data.List as List
import Data.String (codePointFromChar)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Exception (Error, error)
import Util (chunks, indexed, lines, mapFst, pairs, parseInt)

data Element = Number Int | List (List Element)

derive instance Eq Element

instance Ord Element where
  compare (Number a) (Number b) = compare a b
  compare (List a) (List b) = compare a b
  compare a@(Number _) b@(List _) = compare (List (List.singleton a)) b
  compare a@(List _) b@(Number _) = compare a (List (List.singleton b))

parseElement :: List Char -> Either Error (Tuple Element (List Char))
parseElement List.Nil = Left $ error "expected element got ''"
parseElement input@(c : _) | isDecDigit $ codePointFromChar c = do
  n <- parseInt $ fromCharArray $ Array.fromFoldable $ List.takeWhile (codePointFromChar >>> isDecDigit) input
  Right $ Tuple (Number n) (List.dropWhile (codePointFromChar >>> isDecDigit) input)
parseElement ('[' : ']' : rest) = Right $ Tuple (List List.Nil) rest
parseElement ('[' : rest) = do
  Tuple elems rest <- mapFst List.reverse <$> run List.Nil rest
  case rest of
    ']' : rest -> Right $ Tuple (List elems) rest
    rest -> Left $ error $ "expected ] got '" <> fromCharArray (Array.fromFoldable rest) <> "'"
  where
  run :: List Element -> List Char -> Either Error (Tuple (List Element) (List Char))
  run previous rest = do
    Tuple el rest <- parseElement rest
    case rest of
      ',' : rest -> run (el : previous) rest
      rest -> Right $ Tuple (el : previous) rest
parseElement input = Left $ error $ "unexpected input: '" <> fromCharArray (Array.fromFoldable input) <> "'"

parse :: String -> Either Error (Array Element)
parse input = lines input <#> String.trim
  # Array.filter (not <<< String.null)
  <#> toCharArray >>> List.fromFoldable
  # traverse (map fst <<< parseElement)

solvePartOne :: List Element -> Int
solvePartOne elements = pairs elements <#> (\(Tuple a b) -> a < b)
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

partOne :: String -> Either Error String
partOne input = parse input <#> List.fromFoldable >>> solvePartOne >>> show

partTwo :: String -> Either Error String
partTwo input = parse input <#> solvePartTwo >>> show
