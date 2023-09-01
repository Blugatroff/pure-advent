module Year2021.Day21 (day) where

import MeLude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (class MonadError, runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (class MonadState, evalState, get, gets, modify)
import Data.String as String
import Day (makeDay)
import JS.BigInt (BigInt, fromInt)
import Util (dedupCount, maximumOrZero, nonEmptyLines, parseInt, splitStringOnce)

parse :: String -> String |? (Int /\ Int)
parse = nonEmptyLines >>> traverse parseLine >=> case _ of
  [ a, b ] -> Right (a /\ b)
  _ -> Left "expected only two players"

parseLine :: String -> String |? Int
parseLine line = do
  _ /\ numStr <- note ("failed to parse line: " <> line) $ splitStringOnce ":" line
  parseInt $ String.trim numStr

type Player = { position :: Int, points :: Int }

type GameState = { dice :: Int, playerA :: Player, playerB :: Player }

rollDice :: forall m. MonadState GameState m => m Int
rollDice = do
  state <- modify \state -> state { dice = state.dice + 1 }
  pure $ ((state.dice - 1) `mod` 100) + 1

round :: forall m. MonadError Int m => MonadState GameState m => m Unit
round = do
  d1 <- rollDice
  d2 <- rollDice
  d3 <- rollDice
  let m1 = d1 + d2 + d3
  p1 <- map (_.playerA.position) $ modify \state -> state { playerA = state.playerA { position = (state.playerA.position + m1) `mod` 10 } }
  s1 <- map (_.playerA.points) $ modify \state -> state { playerA = state.playerA { points = state.playerA.points + p1 + 1 } }
  when (s1 >= 1000) do
    s2 <- gets _.playerB.points
    die <- map _.dice $ get
    throwError $ s2 * die

  d1 <- rollDice
  d2 <- rollDice
  d3 <- rollDice
  let m2 = d1 + d2 + d3
  p2 <- map (_.playerB.position) $ modify \state -> state { playerB = state.playerB { position = (state.playerB.position + m2) `mod` 10 } }
  s2 <- map (_.playerB.points) $ modify \state -> state { playerB = state.playerB { points = state.playerB.points + p2 + 1 } }
  when (s2 >= 1000) do
    die <- map _.dice $ get
    throwError $ s1 * die

solvePartOne :: Int /\ Int -> String |? String
solvePartOne (a /\ b) = do
  let initialState = { playerA: { position: a - 1, points: 0 }, playerB: { position: b - 1, points: 0 }, dice: 0 }
  case evalState (runExceptT $ forever round) initialState of
    Left result -> Right $ show result
    Right a -> absurd a

possibleDiracs :: Array { value :: Int, frequency :: BigInt }
possibleDiracs = map (\(value /\ frequency) -> { value, frequency: fromInt frequency }) $ dedupCount do
  let sides = [ 1, 2, 3 ]
  d1 <- sides
  d2 <- sides
  d3 <- sides
  pure $ d1 + d2 + d3

playDirac :: Int -> Player /\ Player -> (BigInt /\ BigInt)
playDirac targetPoints = go
  where
  go (a /\ b) = addWinCounts do
    d1 <- possibleDiracs
    let newPositionA = (a.position + d1.value) `mod` 10
    let newA = { position: newPositionA, points: a.points + newPositionA + 1 }
    if newA.points >= targetPoints then pure (d1.frequency /\ zero)
    else do
      possibleDiracs <#> \d2 -> do
        let combinedFrequency = d1.frequency * d2.frequency
        let newPositionB = (b.position + d2.value) `mod` 10
        let newB = { position: newPositionB, points: b.points + newPositionB + 1 }
        if newB.points >= targetPoints then zero /\ combinedFrequency
        else do
          let (winsA /\ winsB) = go (newA /\ newB)
          (winsA * combinedFrequency) /\ (winsB * combinedFrequency)

  addWinCounts :: Array (BigInt /\ BigInt) -> (BigInt /\ BigInt)
  addWinCounts arr = (sum (map fst arr) /\ sum (map snd arr))

solvePartTwo :: (Int /\ Int) -> String |? String
solvePartTwo (a /\ b) = do
  let initialState = { position: a - 1, points: 0 } /\ { position: b - 1, points: 0 }
  let (winsA /\ winsB) = playDirac 21 initialState
  Right $ show $ maximumOrZero [ winsA, winsB ]

day = makeDay parse solvePartOne solvePartTwo
