module Year2022.Day22 (partOne, partTwo) where

import MeLude

import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.Foldable (maximumBy)
import Data.Function (applyN)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Map as M
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.Utils (lines)
import Data.Traversable (sequence, scanl)
import Debug (traceM)
import Util (parseInt, trace)

data Instruction = TurnLeft | TurnRight | Move Int

derive instance genericInstruction :: Generic Instruction _
instance showInstruction :: Show Instruction where
  show = genericShow

type Path = Array Instruction

data Tile = Air | Wall

derive instance genericTile :: Generic Tile _
instance showTile :: Show Tile where
  show = genericShow

instance eqTile :: Eq Tile where
  eq Air Air = true
  eq Wall Wall = true
  eq _ _ = false

type Board = Map (Int /\ Int) Tile

parse :: String -> String |? Board /\ Path
parse input = do
  let trimIfEmptpyAfterTrim s = if String.null (String.trim s) then String.trim s else s
  let theLines = Array.filter (not <<< String.null) $ map trimIfEmptpyAfterTrim $ lines input
  path <- note "failed to parse empty input" $ Array.last theLines
  path <- parsePath path
  board <- parseBoard $ Array.filter (not <<< String.null) $ Array.dropEnd 1 theLines
  pure $ board /\ path

parseBoard :: Array String -> String |? Board
parseBoard lines = M.fromFoldable <<< Array.concat <$> do
  sequence $ flip Array.mapWithIndex lines \y line -> do
    let indexedTiles = Array.mapWithIndex (/\) $ toCharArray line
    let indexedTilesWithoutSpaces = Array.filter (snd >>> notEq ' ') indexedTiles
    for indexedTilesWithoutSpaces \(x /\ tile) -> do
      tile <- parseTile tile
      pure $ ((x + 1) /\ (y + 1)) /\ tile

parseTile :: Char -> String |? Tile
parseTile '.' = Right Air
parseTile '#' = Right Wall
parseTile tile = Left $ "failed to parse tile: " <> show tile

parsePath :: String -> String |? Path
parsePath =
  (splitKeepDelim (String.Pattern "R") >=> splitKeepDelim (String.Pattern "L"))
    >>> Array.filter (not <<< String.null)
    >>> traverse parseInstruction

splitKeepDelim :: String.Pattern -> String -> Array String
splitKeepDelim pattern@(String.Pattern p) = Array.intersperse p <<< String.split pattern

parseInstruction :: String -> String |? Instruction
parseInstruction "L" = Right TurnLeft
parseInstruction "R" = Right TurnRight
parseInstruction n = map Move $ lmap (\_ -> "failed to parse instruction: '" <> n <> "'") $ parseInt n

data Direction = DirUp | DirDown | DirLeft | DirRight

directionX ∷ Direction → Int
directionX DirRight = 1
directionX DirLeft = -1
directionX _ = 0

directionY ∷ Direction → Int
directionY DirDown = 1
directionY DirUp = -1
directionY _ = 0

directionAxis :: forall a. Direction -> a /\ a -> a
directionAxis DirDown = snd
directionAxis DirUp = snd
directionAxis DirRight = fst
directionAxis DirLeft = fst

turnLeft ∷ Direction → Direction
turnLeft DirUp = DirLeft
turnLeft DirLeft = DirDown
turnLeft DirDown = DirRight
turnLeft DirRight = DirUp

turnRight ∷ Direction → Direction
turnRight DirUp = DirRight
turnRight DirRight = DirDown
turnRight DirDown = DirLeft
turnRight DirLeft = DirUp

derive instance genericDirection :: Generic Direction _
instance showDirection :: Show Direction where
  show = genericShow

directionChar :: Direction -> Char
directionChar DirUp = '^'
directionChar DirDown = 'v'
directionChar DirLeft = '<'
directionChar DirRight = '>'

type Cursor = { direction :: Direction, pos :: Int /\ Int }

executeInstruction :: Board -> Cursor -> Instruction -> Cursor
executeInstruction _ cursor TurnLeft = cursor { direction = turnLeft cursor.direction }
executeInstruction _ cursor TurnRight = cursor { direction = turnRight cursor.direction }
executeInstruction board cursor (Move distance) = applyN (moveCursor board) distance cursor

moveCursor :: Board -> Cursor -> Cursor
moveCursor board cursor@{ pos: (px /\ py) } = case M.lookup newPos board of
  Nothing -> cursor { pos = fromMaybe cursor.pos $ wrap cursor }
  Just Air -> cursor { pos = newPos }
  Just Wall -> cursor
  where
  newX = px + directionX cursor.direction
  newY = py + directionY cursor.direction
  newPos = newX /\ newY

  extremes = case cursor.direction of
    DirRight -> minimumBy
    DirLeft -> maximumBy
    DirDown -> minimumBy
    DirUp -> maximumBy

  wrap cursor = bindFlipped (\(pos /\ tile) -> if tile == Air then Just pos else Nothing)
    $ extremes (compare `on` (fst >>> directionAxis cursor.direction))
    $ Array.mapMaybe
        ( \(pos /\ tile) -> case tile of
            Air | onPlane pos -> Just (pos /\ Air)
            Wall | onPlane pos -> Just (pos /\ Wall)
            _ -> Nothing
        )
    $ M.toUnfoldableUnordered board

  onPlane (x /\ y) = case cursor.direction of
    DirRight -> y == py
    DirLeft -> y == py
    DirDown -> x == px
    DirUp -> x == px

findStart :: Board -> Maybe (Int /\ Int)
findStart board = do
  y <- minimum $ map snd $ map fst $ (M.toUnfoldableUnordered board :: Array _)
  x <- minimum $ map fst $ Array.filter (snd >>> eq y) $ map fst $ (M.toUnfoldableUnordered board :: Array _)
  pure $ x /\ y

score :: Cursor -> Int
score { direction, pos: (column /\ row) } = directionScore direction + 1000 * row + 4 * column

directionScore :: Direction -> Int
directionScore = case _ of
  DirRight -> 0
  DirDown -> 1
  DirLeft -> 2
  DirUp -> 3

solvePartOne :: Board /\ Path -> String |? Int
solvePartOne (board /\ path) = do
  start <- note "failed to find start" $ findStart board
  let cursor = { direction: DirRight, pos: start }
  let end = foldl (executeInstruction board) cursor path
  Right $ score end

showBoard :: Map (Int /\ Int) Char -> Board -> String
showBoard markings board = intercalate "\n" $ Array.range minY maxY
  <#> \y -> fromCharArray $ Array.range minX maxX
    <#> \x ->
      M.lookup (x /\ y) markings
        # fromMaybe
            ( case M.lookup (x /\ y) board of
                Nothing -> ' '
                Just Air -> '.'
                Just Wall -> '#'
            )
  where
  assocs = M.toUnfoldableUnordered board :: Array ((Int /\ Int) /\ Tile)
  positions = map fst assocs
  xs = map fst positions
  ys = map snd positions
  minX = fromMaybe 0 $ minimum xs
  minY = fromMaybe 0 $ minimum ys
  maxX = fromMaybe 0 $ maximum xs
  maxY = fromMaybe 0 $ maximum ys

partOne input = parse input >>= solvePartOne >>> map show
partTwo input = parse input # map show
