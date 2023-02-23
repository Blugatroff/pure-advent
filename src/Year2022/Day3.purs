module Year2022.Day3 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.Char (toCharCode)
import Data.CodePoint.Unicode (isAsciiLower, isAsciiUpper)
import Data.List as List
import Data.Set as S
import Data.String (codePointFromChar, null, trim)
import Data.String as String
import Util (chunks, lines, reduceR)

parsePartOne :: String -> Array { before :: String, after :: String }
parsePartOne input = lines input
  <#> trim
  # Array.filter (not <<< null)
  <#> \line -> String.splitAt (String.length line `div` 2) line

parsePartTwo :: String -> List (List String)
parsePartTwo input = lines input <#> trim # List.fromFoldable # List.filter (not <<< null) # chunks 3

findIntersection :: forall input out. Functor input => Foldable input => Monoid (out Char) => Unfoldable out => input String -> out Char
findIntersection lists = lists
  <#> toCharArray
  <#> S.fromFoldable
  # List.fromFoldable
  # reduceR S.intersection
  # maybe mempty S.toUnfoldable

priority :: Char -> Int
priority c
  | isAsciiLower $ codePointFromChar c = toCharCode c - toCharCode 'a' + 1
  | isAsciiUpper $ codePointFromChar c = (toCharCode c - toCharCode 'A') + 27
  | otherwise = 0

solvePartOne :: Array { before :: String, after :: String } -> Int
solvePartOne rucksacks =
  rucksacks
    <#> (\{ before, after } -> [ before, after ])
    >>= findIntersection
    <#> priority
    # sum

solvePartTwo :: List (List String) -> Int
solvePartTwo rucksacks =
  rucksacks
    <#> findIntersection
    # join
    <#> priority
    # sum

partOne :: String -> String |? String
partOne input = parsePartOne input # solvePartOne # show # Right

partTwo :: String -> String |? String
partTwo input = parsePartTwo input # solvePartTwo # show # Right
