module Year2021.Day3 (day) where

import MeLude

import Data.Array.NonEmpty as NonEmptyArray
import Data.List (transpose)
import Data.List as List
import Data.Maybe as Maybe
import Day (makeDay)
import Parsing (Parser, runParser)
import Parsing.Combinators (optional)
import Parsing.Combinators.Array (many, many1)
import Parsing.String (char)
import Util (newline)

data Command = Down Int | Up Int | Forward Int

parseLines :: forall a. Parser String a -> Parser String (Array a)
parseLines p = many do
  a <- p
  optional $ newline
  pure a

parseLine :: Parser String (Array Boolean)
parseLine = map NonEmptyArray.toArray $ many1 $ (const false <$> char '0') <|> (const true <$> char '1')

parser :: Parser String (List (List Boolean))
parser = map List.fromFoldable $ map (map List.fromFoldable) $ parseLines parseLine

parse = lmap show <<< flip runParser parser

mostCommon :: List Boolean -> Boolean
mostCommon list = case compare (List.length yes) (List.length no) of
  EQ -> true
  LT -> false
  GT -> true
  where
  { no, yes } = List.partition identity list

fromBitsRev :: List Boolean -> Int
fromBitsRev List.Nil = 0
fromBitsRev (true:rest) = 1 + fromBitsRev (false:rest)
fromBitsRev (false:rest) = 2 * fromBitsRev rest

fromBits = fromBitsRev <<< List.reverse

solvePartOne :: List (List Boolean) -> Int
solvePartOne lines = fromBits bits * fromBits (map not bits)
  where
  bits = map mostCommon $ transpose lines

oxyBitCriteria :: forall a. List (Boolean /\ a) -> List a
oxyBitCriteria column = map snd $ List.filter (eq mc <<< fst) column
  where
  mc = mostCommon $ map fst column

scrubberBitCriteria :: forall a. List (Boolean /\ a) -> List a
scrubberBitCriteria column = map snd $ List.filter (notEq mc <<< fst) column
  where
  mc = mostCommon $ map fst column

ratingFromBitCriteria :: (List (Boolean /\ List Boolean) -> List (List Boolean)) -> List (List Boolean) -> List Boolean
ratingFromBitCriteria criteria = f 0
  where
  f :: Int -> List (List Boolean) -> List Boolean
  f _ (n:List.Nil) = n
  f cursor numbers = f (cursor + 1) $ criteria $ map (\n -> (Maybe.fromMaybe true (List.index n cursor) /\ n)) numbers
  
oxyRating :: List (List Boolean) -> Int
oxyRating = fromBits <<< ratingFromBitCriteria oxyBitCriteria

scrubberRating :: List (List Boolean) -> Int
scrubberRating = fromBits <<< ratingFromBitCriteria scrubberBitCriteria

solvePartTwo :: List (List Boolean) -> Int
solvePartTwo numbers = oxyRating numbers * scrubberRating numbers

day = makeDay parse
  (Right <<< show <<< solvePartOne)
  (Right <<< show <<< solvePartTwo)

