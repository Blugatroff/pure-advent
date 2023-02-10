module Year2022.Day2 (partOne, partTwo) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.List (List)
import Data.List as List
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import Util (lines, splitStringOnce)

data Shape = Rock | Paper | Scissor

derive instance shapeEq :: Eq Shape
derive instance shapeOrd :: Ord Shape

data Outcome = Win | Lose | Draw

parseShape :: String -> Either Error Shape
parseShape "A" = Right Rock
parseShape "B" = Right Paper
parseShape "C" = Right Scissor
parseShape "X" = Right Rock
parseShape "Y" = Right Paper
parseShape "Z" = Right Scissor
parseShape s = Left $ error $ "Failed to parse " <> s <> " as Rock Paper or Scissor"

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissor = 3

outcomeScore :: Outcome -> Int
outcomeScore Win = 6
outcomeScore Draw = 3
outcomeScore Lose = 0

parseLine :: String -> Either Error (Tuple Shape Shape)
parseLine line = case splitStringOnce " " line of
  Just (Tuple other me) -> do
    otherShape <- parseShape other
    myShape <- parseShape me
    pure $ Tuple otherShape myShape
  Nothing -> Left $ error $ "expected game got " <> line

parse :: String -> Either Error (List (Tuple Shape Shape))
parse input = lines input # List.fromFoldable <#> trim # traverse parseLine

logic :: M.Map (Tuple Shape Shape) Outcome
logic = M.fromFoldable
  [ Tuple (Tuple Rock Rock) Draw
  , Tuple (Tuple Rock Paper) Win
  , Tuple (Tuple Rock Scissor) Lose
  , Tuple (Tuple Paper Rock) Lose
  , Tuple (Tuple Paper Paper) Draw
  , Tuple (Tuple Paper Scissor) Win
  , Tuple (Tuple Scissor Rock) Win
  , Tuple (Tuple Scissor Paper) Lose
  , Tuple (Tuple Scissor Scissor) Draw
  ]

play :: Tuple Shape Shape -> Outcome
play game = M.lookup game logic # fromMaybe Win

solvePartOne :: List (Tuple Shape Shape) -> Int
solvePartOne lines = map gameScore lines # sum
  where
  gameScore :: Tuple Shape Shape -> Int
  gameScore (Tuple a b) = shapeScore b + outcomeScore (play (Tuple a b))

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

solvePartTwo :: List (Tuple Shape Shape) -> Int
solvePartTwo lines = map gameScore lines # sum
  where
  gameScore :: Tuple Shape Shape -> Int
  gameScore (Tuple a b) = score
    where
    chosen = chooseShapeFromOutcome desiredOutcome a
    desiredOutcome = shapeToOutcome b
    outcome = play (Tuple a chosen)
    score = shapeScore chosen + outcomeScore outcome

partOne :: String -> Either Error String
partOne input = parse input <#> solvePartOne <#> show

partTwo :: String -> Either Error String
partTwo input = parse input <#> solvePartTwo <#> show
