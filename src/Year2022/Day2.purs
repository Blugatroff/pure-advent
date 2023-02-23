module Year2022.Day2 (partOne, partTwo) where

import MeLude

import Data.Map as M
import Parsing (Parser, runParser)
import Parsing.Combinators (choice, many, optional, (<??>))
import Parsing.String (char)
import Util (newline, space)

data Shape = Rock | Paper | Scissor

derive instance shapeEq :: Eq Shape
derive instance shapeOrd :: Ord Shape

data Outcome = Win | Lose | Draw

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissor = 3

outcomeScore :: Outcome -> Int
outcomeScore Win = 6
outcomeScore Draw = 3
outcomeScore Lose = 0

shapeParser = "a Shape" <??> choice
  [ char 'A' $> Rock
  , char 'B' $> Paper
  , char 'C' $> Scissor
  , char 'X' $> Rock
  , char 'Y' $> Paper
  , char 'Z' $> Scissor
  ]

parser :: Parser String (List (Shape /\ Shape))
parser = many do
  other <- shapeParser
  space
  me <- shapeParser
  void $ many space
  optional newline
  pure $ other /\ me

parse = lmap show <<< flip runParser parser

logic :: M.Map (Shape /\ Shape) Outcome
logic = M.fromFoldable
  [ (Rock /\ Rock) /\ Draw
  , (Rock /\ Paper) /\ Win
  , (Rock /\ Scissor) /\ Lose
  , (Paper /\ Rock) /\ Lose
  , (Paper /\ Paper) /\ Draw
  , (Paper /\ Scissor) /\ Win
  , (Scissor /\ Rock) /\ Win
  , (Scissor /\ Paper) /\ Lose
  , (Scissor /\ Scissor) /\ Draw
  ]

play :: Shape /\ Shape -> Outcome
play game = M.lookup game logic # fromMaybe Win

solvePartOne :: List (Shape /\ Shape) -> Int
solvePartOne lines = map gameScore lines # sum
  where
  gameScore :: Shape /\ Shape -> Int
  gameScore (a /\ b) = shapeScore b + outcomeScore (play (a /\ b))

chooseShapeFromOutcome :: Outcome -> Shape -> Shape
chooseShapeFromOutcome Draw s = s
chooseShapeFromOutcome Win Rock = Paper
chooseShapeFromOutcome Win Paper = Scissor
chooseShapeFromOutcome Win Scissor = Rock
chooseShapeFromOutcome Lose Rock = Scissor
chooseShapeFromOutcome Lose Paper = Rock
chooseShapeFromOutcome Lose Scissor = Paper

shapeToOutcome :: Shape -> Outcome
shapeToOutcome Rock = Lose
shapeToOutcome Paper = Draw
shapeToOutcome Scissor = Win

solvePartTwo :: List (Shape /\ Shape) -> Int
solvePartTwo lines = map gameScore lines # sum
  where
  gameScore :: Shape /\ Shape -> Int
  gameScore (a /\ b) = score
    where
    chosen = chooseShapeFromOutcome desiredOutcome a
    desiredOutcome = shapeToOutcome b
    outcome = play (a /\ chosen)
    score = shapeScore chosen + outcomeScore outcome

partOne :: String -> String |? String
partOne input = parse input <#> solvePartOne <#> show

partTwo :: String -> String |? String
partTwo input = parse input <#> solvePartTwo <#> show
