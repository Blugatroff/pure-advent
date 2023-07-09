module Year2021.Day2 (day) where

import MeLude

import Day (makeDay)
import Parsing (Parser, runParser)
import Parsing.Combinators (optional, choice)
import Parsing.Combinators.Array (many)
import Parsing.String (char, string)
import Parsing.String.Basic (intDecimal)

data Command = Down Int | Up Int | Forward Int

commandParser :: Parser String (Int -> Command)
commandParser = choice
  [ const Forward <$> string "forward"
  , const Down <$> string "down"
  , const Up <$> string "up"
  ]

parser :: Parser String (Array Command)
parser = many do
  command <- commandParser
  _ <- char ' '
  n <- intDecimal
  optional $ char '\n'
  pure $ command n

parse ∷ String → String |? (Array Command)
parse = lmap show <<< flip runParser parser

solvePartOne :: forall f. Foldable f => f Command -> Int
solvePartOne commands = uncurry (*) $ foldl fold (0 /\ 0) commands
  where
  fold (x /\ y) (Down n) = x /\ (y + n)
  fold (x /\ y) (Up n) = x /\ (y - n)
  fold (x /\ y) (Forward n) = (x + n) /\ y

solvePartTwo :: forall f. Foldable f => f Command -> Int
solvePartTwo commands = x * y
  where
  fold :: { aim :: Int, x :: Int, y :: Int } -> Command -> { aim :: Int, x :: Int, y :: Int }
  fold rec@{ aim } (Down n) = rec { aim = aim + n }
  fold rec@{ aim } (Up n) = rec { aim = aim - n }
  fold { aim, x, y } (Forward n) = { aim, x: x + n, y: y + aim * n }

  { x, y } = foldl fold { aim: 0, x: 0, y: 0 } commands

day = makeDay parse 
  (Right <<< show <<< solvePartOne)
  (Right <<< show <<< solvePartTwo)

