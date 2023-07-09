module Year2022.Day19 (day) where

import MeLude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Heap as Heap
import Data.List as List
import Data.Set as S
import Data.Show.Generic (genericShow)
import Day (makeDay)
import Parsing (Parser, runParser)
import Parsing.Combinators (optional)
import Parsing.Combinators.Array (many)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal)
import Partial.Unsafe (unsafePartial)
import Util (newline)

data Blueprint = Blueprint Int
  { oreRobotCost :: { ore :: Int }
  , clayRobotCost :: { ore :: Int }
  , obsidianRobotCost :: { ore :: Int, clay :: Int }
  , geodeRobotCost :: { ore :: Int, obsidian :: Int }
  , maxOreCost :: Int
  , maxClayCost :: Int
  , maxObsidianCost :: Int
  }

derive instance genericBlueprint :: Generic Blueprint _

instance showBlueprint :: Show Blueprint where
  show = genericShow

blueprintParser :: Parser String Blueprint
blueprintParser = do
  void $ string "Blueprint "
  name <- intDecimal
  void $ string ": Each ore robot costs "
  oreRobotOreCost <- intDecimal
  void $ string " ore. Each clay robot costs "
  clayRobotOreCost <- intDecimal
  void $ string " ore. Each obsidian robot costs "
  obsidianRobotOreCost <- intDecimal
  void $ string " ore and "
  obsidianRobotClayCost <- intDecimal
  void $ string " clay. Each geode robot costs "
  geodeRobotOreCost <- intDecimal
  void $ string " ore and "
  geodeRobotObsidianCost <- intDecimal
  void $ string " obsidian."
  let oreRobotCost = { ore: oreRobotOreCost }
  let clayRobotCost = { ore: clayRobotOreCost }
  let obsidianRobotCost = { ore: obsidianRobotOreCost, clay: obsidianRobotClayCost }
  let geodeRobotCost = { ore: geodeRobotOreCost, obsidian: geodeRobotObsidianCost }
  let maxOreCost = oreRobotCost.ore `max` clayRobotCost.ore `max` obsidianRobotCost.ore `max` geodeRobotCost.ore
  let maxClayCost = obsidianRobotCost.clay
  let maxObsidianCost = geodeRobotCost.obsidian
  pure $ Blueprint name { oreRobotCost, clayRobotCost, obsidianRobotCost, geodeRobotCost, maxOreCost, maxClayCost, maxObsidianCost }

parser :: Parser String (Array Blueprint)
parser = many do
  blueprint <- blueprintParser
  optional newline
  pure blueprint

data Resource = Ore | Clay | Obsidian | Geode

derive instance genericResource :: Generic Resource _
derive instance eqResource :: Eq Resource
derive instance ordResource :: Ord Resource
instance showResource :: Show Resource where
  show = genericShow

allResources = [ Ore, Clay, Obsidian, Geode ]

newtype Robot = Robot Resource

derive instance genericRobot :: Generic Robot _
derive instance eqRobot :: Eq Robot
derive instance ordRobot :: Ord Robot
instance showRobot :: Show Robot where
  show = genericShow

allRobots = map Robot allResources

type InnerState =
  { minutesLeft :: Int
  , geodeRobots :: Int
  , obsidianRobots :: Int
  , clayRobots :: Int
  , oreRobots :: Int
  , availableGeodes :: Int
  , availableObsidian :: Int
  , availableClay :: Int
  , availableOre :: Int
  }

data State = State Blueprint InnerState Int (List Action)

innerState :: State -> InnerState
innerState (State _ inner _ _) = inner

stateProduced :: State -> Int
stateProduced (State _ _ p _) = p

instance eqState :: Eq State where
  eq = eq `on` innerState

instance showState :: Show State where
  show (State _ inner _ previousActions) = show inner <> " " <> show previousActions

fact :: Int -> Int
fact n | n < 0 = 0
fact n = n + fact (n - 1)

facts :: Array Int
facts = map fact $ Array.range 0 30

fastFact :: Int -> Int
fastFact n | n < 0 = 0
fastFact n = unsafePartial $ Array.unsafeIndex facts n

geodeRobot = Robot Geode

mostPossibleGeodes state@(State _ s p _) = estimateWithoutNewRobots + producedFromNewRobots
  where
  estimateWithoutNewRobots = p + (s.minutesLeft * s.geodeRobots)
  -- if a new geode robot would be built every minute
  producedFromNewRobots = if testEnoughResources geodeRobot state then fastFact s.minutesLeft else fastFact (s.minutesLeft - 1)

instance ordState :: Ord State where
  compare ls@(State _ l _ _) rs@(State _ r _ _) = case compare (mostPossibleGeodes ls) (mostPossibleGeodes rs) of
    EQ -> compare l.minutesLeft r.minutesLeft
    r -> r

initialState :: Blueprint -> Int -> State
initialState blueprint minutesLeft = State blueprint
  { minutesLeft
  , availableOre: 0
  , availableClay: 0
  , availableObsidian: 0
  , availableGeodes: 0
  , oreRobots: 1
  , clayRobots: 0
  , obsidianRobots: 0
  , geodeRobots: 0
  }
  0
  List.Nil

data Action = DoNothing | Build Robot

derive instance genericAction :: Generic Action _
instance showAction :: Show Action where
  show DoNothing = "None"
  show (Build (Robot Ore)) = "Ore "
  show (Build (Robot Clay)) = "Clay"
  show (Build (Robot Obsidian)) = "Obsi"
  show (Build (Robot Geode)) = "Geod"

derive instance ordAction :: Ord Action
derive instance eqAction :: Eq Action

testEnoughResources :: Robot -> State -> Boolean
testEnoughResources (Robot Ore) (State (Blueprint _ { oreRobotCost }) state _ _) = state.availableOre >= oreRobotCost.ore
testEnoughResources (Robot Clay) (State (Blueprint _ { clayRobotCost }) state _ _) = state.availableOre >= clayRobotCost.ore
testEnoughResources (Robot Obsidian) (State (Blueprint _ { obsidianRobotCost }) state _ _) = state.availableOre >= obsidianRobotCost.ore && state.availableClay >= obsidianRobotCost.clay
testEnoughResources (Robot Geode) (State (Blueprint _ { geodeRobotCost }) state _ _) = state.availableOre >= geodeRobotCost.ore && state.availableObsidian >= geodeRobotCost.obsidian

useResources :: Robot -> State -> Maybe State
useResources (Robot Ore) (State b@(Blueprint _ { oreRobotCost }) state p a) = if state.availableOre < oreRobotCost.ore then Nothing else Just $ State b (state { availableOre = state.availableOre - oreRobotCost.ore }) p a
useResources (Robot Clay) (State b@(Blueprint _ { clayRobotCost }) state p a) = if state.availableOre < clayRobotCost.ore then Nothing else Just $ State b (state { availableOre = state.availableOre - clayRobotCost.ore }) p a
useResources (Robot Obsidian) (State b@(Blueprint _ { obsidianRobotCost }) state p a) = if state.availableOre < obsidianRobotCost.ore || state.availableClay < obsidianRobotCost.clay then Nothing else Just $ State b (state { availableOre = state.availableOre - obsidianRobotCost.ore, availableClay = state.availableClay - obsidianRobotCost.clay }) p a
useResources (Robot Geode) (State b@(Blueprint _ { geodeRobotCost }) state p a) = if state.availableOre < geodeRobotCost.ore || state.availableObsidian < geodeRobotCost.obsidian then Nothing else Just $ State b (state { availableOre = state.availableOre - geodeRobotCost.ore, availableObsidian = state.availableObsidian - geodeRobotCost.obsidian }) p a

simulate :: State -> Action -> Maybe State
simulate state action = simulateFactory $ advanceTime $ appendAction state
  where
  collectMinerals (State b state p a) = State b
    ( state
        { availableOre = state.availableOre + state.oreRobots
        , availableClay = state.availableClay + state.clayRobots
        , availableObsidian = state.availableObsidian + state.obsidianRobots
        , availableGeodes = state.availableGeodes + state.geodeRobots
        }
    )
    (p + state.geodeRobots)
    a

  advanceTime (State b state p a) = State b (state { minutesLeft = state.minutesLeft - 1 }) p a
  appendAction (State b state p actions) = State b state p (action : actions)

  simulateFactory = case action of
    DoNothing -> Just <<< collectMinerals
    Build robot -> map (addRobot robot <<< collectMinerals) <<< useResources robot

  addRobot (Robot Ore) (State b state p a) = State b (state { oreRobots = state.oreRobots + 1 }) p a
  addRobot (Robot Clay) (State b state p a) = State b (state { clayRobots = state.clayRobots + 1 }) p a
  addRobot (Robot Obsidian) (State b state p a) = State b (state { obsidianRobots = state.obsidianRobots + 1 }) p a
  addRobot (Robot Geode) (State b state p a) = State b (state { geodeRobots = state.geodeRobots + 1 }) p a

allActions = map Build (Array.reverse allRobots) <> [ DoNothing ]

tryActionIfUseful :: State -> Action -> Maybe State
tryActionIfUseful state@(State (Blueprint _ { maxOreCost, maxClayCost, maxObsidianCost }) s _ _) = case _ of
  (Build (Robot Ore)) | s.oreRobots >= maxOreCost -> Nothing
  (Build (Robot Clay)) | s.clayRobots >= maxClayCost -> Nothing
  (Build (Robot Obsidian)) | s.obsidianRobots >= maxObsidianCost -> Nothing
  action -> simulate state action

solveBlueprint :: Int -> Blueprint -> State
solveBlueprint minutesLeft blueprint = f S.empty 0 (Heap.singleton init)
  where
  init = initialState blueprint minutesLeft

  f :: Set InnerState -> Int -> Heap.Heap Heap.Max State -> State
  f seen mostGeodes queue = case Heap.takeMax queue of
    Nothing -> init
    Just (state@(State _ s _ _) /\ _) | s.minutesLeft == 0 -> state
    Just ((State _ s _ _) /\ queue) | S.member s seen -> f seen mostGeodes queue
    Just (state@(State _ s p _) /\ queue) ->
      let
        newSeen = S.insert s seen
        newMostGeodes = max mostGeodes p
        tryActionIfUsefulHere = tryActionIfUseful state
        folding queue action = forEachPossibleState newMostGeodes seen queue (tryActionIfUsefulHere action)
        newQueue = foldl folding queue allActions
      in
        f newSeen newMostGeodes newQueue

  forEachPossibleState :: Int -> Set InnerState -> Heap.Heap Heap.Max State -> Maybe State -> Heap.Heap Heap.Max State
  forEachPossibleState _ _ queue Nothing = queue
  forEachPossibleState _ seen queue (Just nextState) | S.member (innerState nextState) seen = queue
  forEachPossibleState mostGeodes _ queue (Just nextState) | mostPossibleGeodes nextState < mostGeodes = queue
  forEachPossibleState _ _ queue (Just nextState) = Heap.insert nextState queue

solvePartOne ∷ Array Blueprint → Int
solvePartOne blueprints = sum $ map scoreBlueprint blueprints
  where
  scoreBlueprint blueprint@(Blueprint id _) = id * stateProduced (solveBlueprint 24 blueprint)

solvePartTwo ∷ Array Blueprint → Int
solvePartTwo blueprints = product $ map (stateProduced <<< solveBlueprint 32) $ Array.slice 0 3 blueprints

day = makeDay (flip runParser parser >>> lmap show)
  (Right <<< show <<< solvePartOne)
  (Right <<< show <<< solvePartTwo)
