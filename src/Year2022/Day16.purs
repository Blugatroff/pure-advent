module Year2022.Day16 (partOne, partTwo) where

import MeLude

import Control.Apply ((*>))
import Data.Array as Array
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.HashMap as M
import Data.HashSet as S
import Parsing (Parser, runParser)
import Parsing.Combinators (many, optional, replicateM, sepBy1)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, upper)
import Util (newline, tuplePermutations)

type Valve = { name :: String, rate :: Int, tunnels :: List String }

type Valves = Array Valve

valveNameParser :: Parser String String
valveNameParser = fromCharArray <<< Array.fromFoldable <$> replicateM 2 upper

valveParser :: Parser String Valve
valveParser = do
  _ <- string "Valve "
  name <- valveNameParser
  _ <- string " has flow rate="
  rate <- intDecimal
  _ <- string "; tunnel" *> optional (string "s")
  _ <- string " lead" *> optional (string "s")
  _ <- string " to valve" *> optional (string "s")
  _ <- string " "
  tunnels <- NEL.toList <$> sepBy1 valveNameParser (string ", ")
  pure { name, rate, tunnels }

parser :: Parser String Valves
parser = Array.fromFoldable <$> many do
  valve <- valveParser
  optional newline
  pure valve

type Network = HashMap String Valve

getValve :: String -> Network -> Valve
getValve name = fromMaybe { name: "NOTFOUND", rate: 0, tunnels: List.Nil } <<< M.lookup name

type Path = List (String /\ String)
type Paths = HashMap String (HashMap String Path)

findPaths :: Network -> HashMap String (HashMap String Path)
findPaths network = network <#> \valve -> findPathsFrom valve
  where
  findPathsFrom valve =
    let
      current = M.fromFoldable [ valve.name /\ List.Nil ]
    in
      go { current, connections: current, next: M.empty }

  go :: { next :: HashMap String Path, current :: HashMap String Path, connections :: HashMap String Path } -> HashMap String Path
  go { current, connections } | M.isEmpty current = connections
  go { current, connections } = go $ newA { current = newA.next, next = M.empty }
    where
    newA = foldl fold { next: M.empty, current, connections } $ M.toArrayBy (/\) current

  fold { connections, current, next } (name /\ path) = foldl fold { connections, current, next } $ (getValve name network).tunnels
    where
    fold { connections, current, next } tunnel = case M.lookup tunnel connections of
      Nothing ->
        let
          connPath = path <> List.singleton (name /\ tunnel)
        in
          { connections: M.insert tunnel connPath connections, current, next: M.insert tunnel connPath next }
      Just _ -> { connections, current, next }

createNetwork :: forall f. Functor f => Foldable f => f Valve -> Network
createNetwork = M.fromFoldable <<< map \valve -> valve.name /\ valve

type Move =
  { reward :: Int
  , target :: String
  , path :: Path
  }

moveCost :: Move -> Int
moveCost { path } = List.length path + 1

type State =
  { network :: Network
  , paths :: Paths
  , position :: String
  , maxTurns :: Int
  , turn :: Int
  , pressure :: Int
  , openValves :: HashSet String
  }

turnsLeft :: State -> Int
turnsLeft { maxTurns, turn } = maxTurns - turn

getPaths :: String -> Paths -> HashMap String Path
getPaths src = fromMaybe M.empty <<< M.lookup src

findNextMoves :: State -> List Move
findNextMoves state =
  getPaths state.position state.paths
    # M.toArrayBy (/\)
    # List.fromFoldable
    # List.mapMaybe \(name /\ path) -> do
        if S.member name state.openValves then Nothing else pure unit
        let flow = (getValve name state.network).rate
        if flow == 0 then Nothing else pure unit
        let travelTurns = List.length path
        let openTurns = 1
        let turnsSpentOpen = turnsLeft state - travelTurns - openTurns
        if turnsSpentOpen < 0 then Nothing
        else pure { reward: flow * turnsSpentOpen, target: name, path }

applyMove :: Move -> State -> State
applyMove move state = state
  { position = move.target
  , turn = state.turn + moveCost move
  , pressure = state.pressure + move.reward
  , openValves = S.insert move.target state.openValves
  }

type ValvesOpen = HashSet String
type BestPressureAchieved = Int
type Best = HashMap ValvesOpen BestPressureAchieved

applyBestMoves :: (State -> Best -> Best) -> State -> Best -> State /\ List Move /\ Best
applyBestMoves bestUpdater state best = spreadFrom List.Nil state best $ findNextMoves state
  where
  spreadFrom :: List Move -> State -> Best -> List Move -> State /\ List Move /\ Best
  spreadFrom bestMoves bestState best = case _ of
    List.Nil -> bestState /\ bestMoves /\ bestUpdater state best
    (move : moves) ->
      let
        next = applyMove move state
        (next /\ nextMoves /\ newBest) = applyBestMoves bestUpdater next best
      in
        if next.pressure > bestState.pressure then
          spreadFrom nextMoves next newBest moves
        else
          spreadFrom bestMoves bestState newBest moves

solvePartOne :: Network -> Paths -> Int
solvePartOne network paths = _.pressure $ fst $ applyBestMoves (const identity) (startingState network paths 30) M.empty

startingState :: Network -> Paths -> Int -> State
startingState network paths maxTurns = { network, paths, position: "AA", maxTurns, turn: 0, pressure: 0, openValves: S.empty }

solvePartTwo :: Network -> Paths -> Int
solvePartTwo network paths = fromMaybe (-1) $ bestPressure
  where
  bestUpdater state best = flip (flip M.alter state.openValves) best case _ of
    Nothing -> Just state.pressure
    Just p | p < state.pressure -> Just state.pressure
    Just p -> Just p

  (_ /\ _ /\ best) = applyBestMoves bestUpdater (startingState network paths 26) M.empty

  bestPressure = M.toArrayBy (/\) best
    # tuplePermutations
    # Array.filter (\((human /\ _) /\ (elephant /\ _)) -> S.isEmpty $ S.intersection human elephant)
    # map (\((_ /\ human) /\ (_ /\ elephant)) -> human + elephant)
    # maximum

part :: (Network -> Paths -> Int) -> String -> String |? String
part solve input = do
  valves <- lmap show $ runParser input parser
  let network = createNetwork valves
  let paths = findPaths network
  pure $ show $ solve network paths

partOne ∷ String → String |? String
partOne = part solvePartOne

partTwo ∷ String → String |? String
partTwo = part solvePartTwo
