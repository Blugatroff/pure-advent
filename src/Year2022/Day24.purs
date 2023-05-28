module Year2022.Day24 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.Direction (Direction(..), allDirections, directionX, directionY)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List.Lazy as LazyList
import Data.Map as M
import Data.Pos (Pos(..), x, y)
import Data.Primitive (class PrimitiveKey, primitiveKey)
import Data.Show.Generic (genericShow)
import Data.TraversableWithIndex (forWithIndex)
import Dijkstra (class World, Cell(..), findPath)
import Util (nonEmptyLines)

type Tile = Array Direction

type Valley =
  { blizzards :: Map Pos Tile -- every position not in the map is a wall
  , start :: Pos
  , dest :: Pos
  , end :: { x :: Int, y :: Int }
  , minute :: Int
  }

parse :: String -> String |? Valley
parse input = do
  lines <- forWithIndex (nonEmptyLines input) \y line -> Array.catMaybes <$> do
    forWithIndex (toCharArray line) \x c -> map ((Pos x y) /\ _) <$> do
      case c of
        '#' -> Right $ Nothing
        '.' -> Right $ Just []
        '^' -> Right $ Just [ DirUp ]
        '>' -> Right $ Just [ DirRight ]
        'v' -> Right $ Just [ DirDown ]
        '<' -> Right $ Just [ DirLeft ]
        c -> Left $ "Failed too parse tile " <> show c
  start <- note "Failed to find start" $ map fst (Array.head lines >>= Array.find (snd >>> eq []))
  dest <- note "Failed to find end" $ map fst (Array.last lines >>= Array.find (snd >>> eq []))
  let assocs = Array.concat lines
  let blizzards = M.fromFoldable assocs
  let end = { x: x dest + 1, y: y dest }
  pure { start, dest, blizzards, end, minute: 0 }

moveBlizzards :: Valley -> Valley
moveBlizzards valley = valley { blizzards = newBlizzards, minute = valley.minute + 1 }
  where
  newBlizzards = foldl fold M.empty newBlizzardPositions

  fold m (pos /\ blizzards) = M.insertWith (<>) pos blizzards m

  newBlizzardPositions :: Array (Pos /\ (Array Direction))
  newBlizzardPositions = do
    (pos /\ blizzards) <- M.toUnfoldableUnordered valley.blizzards
    case blizzards of
      [] -> pure $ pos /\ []
      blizzards -> moveBlizzard pos =<< blizzards

  moveBlizzard pos@(Pos x y) blizzard = [ pos /\ [], wrapPos newPos /\ [ blizzard ] ]
    where
    newPos = Pos (x + directionX blizzard) (y + directionY blizzard)

    wrapPos (Pos x y) | x <= 0 = wrapPos $ Pos (valley.end.x - 1) y
    wrapPos (Pos x y) | y <= 0 = wrapPos $ Pos x (valley.end.y - 1)
    wrapPos (Pos x y) | x >= valley.end.x = wrapPos $ Pos 1 y
    wrapPos (Pos x y) | y >= valley.end.y = wrapPos $ Pos x 1
    wrapPos pos = pos

data Action = Wait | Move Direction

derive instance genericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow

instance eqAction :: Eq Action where
  eq = genericEq

possibleActions :: Valley -> Pos -> Array Action
possibleActions valley pos@(Pos x y) =
  ( allDirections
      # Array.mapMaybe \dir -> do
          let newPos = Pos (x + directionX dir) (y + directionY dir)
          blizzards <- M.lookup newPos valley.blizzards
          case blizzards of
            [] -> Just $ Move dir
            _ -> Nothing
  ) <> case M.lookup pos valley.blizzards of
    Nothing -> []
    Just [] -> [ Wait ]
    Just _ -> []

data Phase = ToGoalFirst | BackToStart | ToGoalSecond

derive instance eqPhase :: Eq Phase
derive instance ordPhase :: Ord Phase

instance showPhase :: Show Phase where
  show ToGoalFirst = "1"
  show BackToStart = "2"
  show ToGoalSecond = "3"

data ValleyWorld = ValleyWorld
data ValleyWorldPos = ValleyWorldPos Phase Pos (LazyList.List Valley)

derive instance genericValleyWorldPos :: Generic ValleyWorldPos _
derive instance eqValleyWorldPos :: Eq ValleyWorldPos
instance ordValleyWorld :: Ord ValleyWorldPos where
  compare (ValleyWorldPos phaseL posL valleysL) (ValleyWorldPos phaseR posR valleysR) = case compare phaseL phaseR of
    EQ -> case compare posL posR of
      EQ -> compare (_.minute <$> LazyList.head valleysL) (_.minute <$> LazyList.head valleysR)
      c -> c
    c -> c

instance showValleyWorldPos :: Show ValleyWorldPos where
  show (ValleyWorldPos phase pos valleys) = "(ValleyWorldPos " <> show phase <> " " <> maybe "" (_.minute >>> show) (LazyList.head valleys) <> " " <> show pos <> ")"

instance primitiveKeyValleyWorldPos :: PrimitiveKey ValleyWorldPos String where
  primitiveKey (ValleyWorldPos phase pos valleys) = show phase <> "|" <> maybe "" (_.minute >>> show) (LazyList.head valleys) <> "|" <> show (primitiveKey pos)

instance valleyWorldWorld :: World ValleyWorld ValleyWorldPos String where
  lookupCell (ValleyWorldPos phase pos valleys) ValleyWorld = do
    valley <- LazyList.head valleys
    _ <- M.lookup pos valley.blizzards
    case phase of
      ToGoalSecond | valley.dest == pos -> Just $ Destination 1
      _ -> Just $ Cell 1

  adjacentCells (ValleyWorldPos phase pos@(Pos x y) valleys) ValleyWorld = do
    valley <- maybe [] Array.singleton $ LazyList.head valleys
    nextValleys <- maybe [] Array.singleton $ LazyList.tail valleys
    possibleActions valley pos <#> \action -> case action of
      Wait -> ValleyWorldPos phase pos nextValleys
      Move direction -> do
        let newPos = Pos (x + directionX direction) (y + directionY direction)
        let newPhase = (case phase of
              ToGoalFirst | valley.dest == newPos -> BackToStart
              BackToStart | valley.start == newPos -> ToGoalSecond
              phase -> phase)
        ValleyWorldPos newPhase newPos nextValleys

solve :: Phase -> Valley -> String
solve startPhase valley = do
  let valleys = LazyList.iterate moveBlizzards valley
  let startPos = ValleyWorldPos startPhase valley.start valleys
  let solution = findPath ValleyWorld startPos
  maybe "No Solution Found!" (_.cost >>> (_ - 1) >>> show) solution

partOne = parse >>> map (solve ToGoalSecond)
partTwo = parse >>> map (solve ToGoalFirst)

