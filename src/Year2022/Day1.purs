module Year2022.Day1 (day) where

import MeLude

import Data.Array as Array
import Day (makeDay)
import Parsing (Parser, runParser)
import Parsing.Combinators (many, optional)
import Parsing.Combinators.Array (many1)
import Parsing.String (char)
import Parsing.String.Basic (intDecimal)

parser :: Parser String (Array (Array Int))
parser = map Array.fromFoldable $ many do
  cals <- map Array.fromFoldable $ many1 do
    cal <- intDecimal
    optional $ char '\n'
    pure cal
  optional $ char '\n'
  pure cals

parse ∷ String → String |? (Array (Array Int))
parse = lmap show <<< flip runParser parser

solvePartOne :: Array (Array Int) -> Int
solvePartOne = map sum >>> maximum >>> fromMaybe (-1)

solvePartTwo :: Array (Array Int) -> Int
solvePartTwo = map sum >>> Array.sort >>> Array.takeEnd 3 >>> sum

day = makeDay parse
  (Right <<< show <<< solvePartOne)
  (Right <<< show <<< solvePartTwo)
