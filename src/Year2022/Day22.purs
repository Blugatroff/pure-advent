module Year2022.Day22 (day) where

import MeLude

import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.Direction (Direction(..), directionX, directionY, turnLeft, turnRight)
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.Utils (lines)
import Day (makeDay)
import Util (maximumOrZero, minimumOrZero, parseInt)

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
      pure $ (x /\ y) /\ tile

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

directionAxis :: forall a. Direction -> a /\ a -> a
directionAxis DirDown = snd
directionAxis DirUp = snd
directionAxis DirRight = fst
directionAxis DirLeft = fst

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
score { direction, pos: (column /\ row) } = directionScore direction + 1000 * (row + 1) + 4 * (column + 1)

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

type CubeFace = Map (Int /\ Int) Tile

type Cube =
  { faces :: Map (Int /\ Int) CubeFace
  , connections :: Connections
  , board :: Board
  , faceSize :: Int
  }

type Connections = Map ((Int /\ Int) /\ Direction) ((Int /\ Int) /\ Direction)

buildCube :: Connections -> Board -> Cube
buildCube connections board = { connections, faces, board, faceSize }
  where
  keys = fst <$> M.toUnfoldableUnordered board :: Array (Int /\ Int)
  xs = fst <$> keys
  ys = snd <$> keys
  totalWidth = maximumOrZero xs - minimumOrZero xs + 1
  totalHeight = maximumOrZero ys - minimumOrZero ys + 1
  faceSize = gcd totalWidth totalHeight
  width = totalWidth `div` faceSize
  height = totalHeight `div` faceSize
  faces = M.fromFoldable $ Array.concat do
    (<#>) (Array.range 0 (width - 1)) \faceX -> Array.catMaybes do
      (<#>) (Array.range 0 (height - 1)) \faceY -> do
        ((faceX /\ faceY) /\ _) <<< M.fromFoldable <<< Array.concat <$> do
          for (Array.range 0 (faceSize - 1)) \fx -> do
            for (Array.range 0 (faceSize - 1)) \fy -> do
              let x = faceX * faceSize + fx
              let y = faceY * faceSize + fy
              tile <- M.lookup (x /\ y) board
              let rx = x `mod` faceSize
              let ry = y `mod` faceSize
              pure $ (rx /\ ry) /\ tile

type FaceLocation = (Int /\ Int) /\ Direction

connectionsTestInput :: Connections
connectionsTestInput = M.fromFoldable do
  [ ((2 /\ 0) /\ DirUp) /\ ((0 /\ 1) /\ DirDown)
  , ((2 /\ 0) /\ DirRight) /\ ((3 /\ 2) /\ DirLeft)
  , ((2 /\ 0) /\ DirDown) /\ ((2 /\ 1) /\ DirDown)
  , ((2 /\ 0) /\ DirLeft) /\ ((1 /\ 1) /\ DirDown)

  , ((0 /\ 1) /\ DirUp) /\ ((2 /\ 0) /\ DirDown)
  , ((0 /\ 1) /\ DirRight) /\ ((1 /\ 1) /\ DirRight)
  , ((0 /\ 1) /\ DirDown) /\ ((2 /\ 2) /\ DirUp)
  , ((0 /\ 1) /\ DirLeft) /\ ((3 /\ 2) /\ DirUp)

  , ((1 /\ 1) /\ DirUp) /\ ((2 /\ 0) /\ DirRight)
  , ((1 /\ 1) /\ DirRight) /\ ((2 /\ 1) /\ DirRight)
  , ((1 /\ 1) /\ DirDown) /\ ((2 /\ 2) /\ DirRight)
  , ((1 /\ 1) /\ DirLeft) /\ ((0 /\ 1) /\ DirLeft)

  , ((2 /\ 1) /\ DirUp) /\ ((2 /\ 0) /\ DirUp)
  , ((2 /\ 1) /\ DirRight) /\ ((3 /\ 2) /\ DirDown)
  , ((2 /\ 1) /\ DirDown) /\ ((2 /\ 2) /\ DirDown)
  , ((2 /\ 1) /\ DirLeft) /\ ((1 /\ 1) /\ DirLeft)

  , ((2 /\ 2) /\ DirUp) /\ ((2 /\ 1) /\ DirUp)
  , ((2 /\ 2) /\ DirRight) /\ ((3 /\ 2) /\ DirRight)
  , ((2 /\ 2) /\ DirDown) /\ ((0 /\ 1) /\ DirUp)
  , ((2 /\ 2) /\ DirLeft) /\ ((1 /\ 1) /\ DirUp)

  , ((3 /\ 2) /\ DirUp) /\ ((2 /\ 1) /\ DirLeft)
  , ((3 /\ 2) /\ DirRight) /\ ((2 /\ 0) /\ DirLeft)
  , ((3 /\ 2) /\ DirDown) /\ ((0 /\ 1) /\ DirRight)
  , ((3 /\ 2) /\ DirLeft) /\ ((2 /\ 2) /\ DirLeft)
  ]

connectionsRealInput :: Connections
connectionsRealInput = M.fromFoldable do
  [ ((1 /\ 0) /\ DirUp) /\ ((0 /\ 3) /\ DirRight)
  , ((1 /\ 0) /\ DirRight) /\ ((2 /\ 0) /\ DirRight)
  , ((1 /\ 0) /\ DirDown) /\ ((1 /\ 1) /\ DirDown)
  , ((1 /\ 0) /\ DirLeft) /\ ((0 /\ 2) /\ DirRight)

  , ((2 /\ 0) /\ DirUp) /\ ((0 /\ 3) /\ DirUp)
  , ((2 /\ 0) /\ DirRight) /\ ((1 /\ 2) /\ DirLeft)
  , ((2 /\ 0) /\ DirDown) /\ ((1 /\ 1) /\ DirLeft)
  , ((2 /\ 0) /\ DirLeft) /\ ((1 /\ 0) /\ DirLeft)

  , ((1 /\ 1) /\ DirUp) /\ ((1 /\ 0) /\ DirUp)
  , ((1 /\ 1) /\ DirRight) /\ ((2 /\ 0) /\ DirUp)
  , ((1 /\ 1) /\ DirDown) /\ ((1 /\ 2) /\ DirDown)
  , ((1 /\ 1) /\ DirLeft) /\ ((0 /\ 2) /\ DirDown)

  , ((0 /\ 2) /\ DirUp) /\ ((1 /\ 1) /\ DirRight)
  , ((0 /\ 2) /\ DirRight) /\ ((1 /\ 2) /\ DirRight)
  , ((0 /\ 2) /\ DirDown) /\ ((0 /\ 3) /\ DirDown)
  , ((0 /\ 2) /\ DirLeft) /\ ((1 /\ 0) /\ DirRight)

  , ((1 /\ 2) /\ DirUp) /\ ((1 /\ 1) /\ DirUp)
  , ((1 /\ 2) /\ DirRight) /\ ((2 /\ 0) /\ DirLeft)
  , ((1 /\ 2) /\ DirDown) /\ ((0 /\ 3) /\ DirLeft)
  , ((1 /\ 2) /\ DirLeft) /\ ((0 /\ 2) /\ DirLeft)

  , ((0 /\ 3) /\ DirUp) /\ ((0 /\ 2) /\ DirUp)
  , ((0 /\ 3) /\ DirRight) /\ ((1 /\ 2) /\ DirUp)
  , ((0 /\ 3) /\ DirDown) /\ ((2 /\ 0) /\ DirDown)
  , ((0 /\ 3) /\ DirLeft) /\ ((1 /\ 0) /\ DirDown)
  ]

type CubeCursor = { direction :: Direction, face :: Int /\ Int, pos :: Int /\ Int }

scoreOnCube :: Cube -> CubeCursor -> Int
scoreOnCube { faceSize } { direction, pos: (x /\ y), face: (fx /\ fy) } = do
  let column = x + fx * faceSize + 1
  let row = y + fy * faceSize + 1
  directionScore direction + 1000 * row + 4 * column

executeInstructionOnCube :: Cube -> CubeCursor -> Instruction -> CubeCursor
executeInstructionOnCube _ cursor TurnLeft = cursor { direction = turnLeft cursor.direction }
executeInstructionOnCube _ cursor TurnRight = cursor { direction = turnRight cursor.direction }
executeInstructionOnCube cube cursor (Move distance) = applyN (moveCursorOnCube cube) distance cursor

moveCursorOnCube :: Cube -> CubeCursor -> CubeCursor
moveCursorOnCube cube cursor@{ face, pos: (px /\ py) } = fromMaybe cursor do
  face <- M.lookup face cube.faces
  case M.lookup newPos face of
    Nothing -> wrap cursor
    Just Air -> pure $ cursor { pos = newPos }
    Just Wall -> pure cursor
  where
  newX = px + directionX cursor.direction
  newY = py + directionY cursor.direction
  newPos = newX /\ newY

  wrap :: CubeCursor -> Maybe CubeCursor
  wrap cursor = do
    (face /\ direction) <- M.lookup (face /\ cursor.direction) cube.connections
    let maxFace = cube.faceSize - 1
    let (cursorX /\ cursorY) = cursor.pos
    let
      newPos =
        ( case cursor.direction, direction of
            DirUp, DirDown -> (maxFace - cursorX) /\ cursorY
            DirRight, DirLeft -> cursorX /\ (maxFace - cursorY)
            DirDown, DirUp -> (maxFace - cursorX) /\ cursorY
            DirLeft, DirRight -> cursorX /\ (maxFace - cursorY)

            DirUp, DirUp -> cursorX /\ maxFace
            DirRight, DirRight -> 0 /\ cursorY
            DirDown, DirDown -> cursorX /\ 0
            DirLeft, DirLeft -> maxFace /\ cursorY

            DirUp, DirRight -> 0 /\ cursorX
            DirUp, DirLeft -> maxFace /\ (maxFace - cursorX)
            DirRight, DirDown -> (maxFace - cursorY) /\ 0
            DirRight, DirUp -> cursorY /\ maxFace
            DirDown, DirLeft -> maxFace /\ cursorX
            DirDown, DirRight -> 0 /\ (maxFace - cursorX)
            DirLeft, DirUp -> (maxFace - cursorY) /\ maxFace
            DirLeft, DirDown -> cursorY /\ 0
        )
    M.lookup face cube.faces >>= M.lookup newPos >>> case _ of
      Nothing -> Nothing
      Just Wall -> Nothing
      Just Air -> pure $ cursor { pos = newPos, direction = direction, face = face }

findStartOnCube :: Cube -> Maybe ((Int /\ Int) /\ (Int /\ Int))
findStartOnCube { board, faceSize } = do
  y <- minimum $ map snd $ map fst $ (M.toUnfoldableUnordered board :: Array _)
  x <- minimum $ map fst $ Array.filter (snd >>> eq y) $ map fst $ (M.toUnfoldableUnordered board :: Array _)
  let fx = x `div` faceSize
  let fy = y `div` faceSize
  let rx = x `mod` faceSize
  let ry = y `mod` faceSize
  pure $ (fx /\ fy) /\ (rx /\ ry)

solvePartTwo :: Board /\ Path -> String |? Int
solvePartTwo (board /\ path) = do
  let width = fromMaybe 0 $ maximum $ map fst $ (S.toUnfoldable :: (_ -> Array _)) $ M.keys board
  let connections = if width > 100 then connectionsRealInput else connectionsTestInput
  let cube = buildCube connections board
  face /\ start <- note "failed to find start" $ findStartOnCube cube
  let startCursor = { direction: DirRight, pos: start, face }
  let end = foldl (executeInstructionOnCube cube) startCursor path
  Right $ scoreOnCube cube end

day = makeDay parse
  (map show <<< solvePartOne)
  (map show <<< solvePartTwo)
