module Year2021.Day1 (day) where

import MeLude

import Data.Array.NonEmpty as NonEmptyArray
import Data.List as List
import Data.NaturalTransformation (type (~>))
import Data.NonEmpty (NonEmpty(..), (:|))
import Day (Day, makeDay)
import Parsing (Parser, runParser)
import Parsing.Combinators (optional)
import Parsing.Combinators.Array (many1)
import Parsing.String (char)
import Parsing.String.Basic (intDecimal)
import Util (windowsNonEmpty)

parser :: Parser String (NonEmpty Array Int)
parser = map NonEmptyArray.toNonEmpty $ many1 do
  cal <- intDecimal
  optional $ char '\n'
  pure cal

parse ∷ String → String |? (NonEmpty Array Int)
parse = lmap show <<< flip runParser parser

solvePartOne :: forall f. Foldable f => NonEmpty f Int -> Int
solvePartOne (head :| rest) = snd $ foldl fold (head /\ 0) rest
  where
  fold (p /\ c) v | v > p = v /\ (c + 1)
  fold (_ /\ c) v = v /\ c

solvePartTwo :: NonEmpty List Int -> Int
solvePartTwo = solvePartOne <<< map sum <<< windowsNonEmpty 3

nonEmptyArrayToList :: NonEmpty Array ~> NonEmpty List
nonEmptyArrayToList (NonEmpty a arr) = NonEmpty a $ List.fromFoldable arr

day :: Day
day = makeDay parse 
  (Right <<< show <<< solvePartOne) 
  (Right <<< show <<< solvePartTwo <<< nonEmptyArrayToList)

