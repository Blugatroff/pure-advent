module Year2022.Day14 (partOne, partTwo) where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.State (State, evalState, get, modify, modify_)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.List as List
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (class Foldable, for, for_, maximum, minimum, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Exception (error)
import Util (lines, parseInt, splitStringOnce, trace, windows2)

type Pos = { x :: Int, y :: Int }

type Path = Array Pos

parsePath line =
  String.split (String.Pattern "->") line
    <#> String.trim
    <#> splitStringOnce ","
    # traverse case _ of
        Just (Tuple x y) -> do
          x <- parseInt x
          y <- parseInt y
          Right { x, y }
        Nothing -> Left $ error $ "failed to parse path: " <> line

parse input = lines input <#> String.trim # Array.filter (not <<< String.null) # traverse parsePath

data Block = Stone | Sand

derive instance eqBlock :: Eq Block

type Cave = M.Map Pos Block

insertStone { x: x1, y: y1 } { x: x2, y: y2 } | y1 == y2 = for_ (Array.range x1 x2) $ \x -> modify $ M.insert { x, y: y1 } Stone
insertStone { x: x1, y: y1 } { x: x2, y: y2 } | x1 == x2 = for_ (Array.range y1 y2) $ \y -> modify $ M.insert { x: x1, y } Stone
insertStone _ _ = pure unit

insertPath path =
  for_ (windows2 $ List.fromFoldable path)
    \(Tuple from to) -> insertStone from to

insertPaths paths = for_ paths insertPath

drawCave cave = String.joinWith "\n" $
  Array.range minY maxY <#> \y ->
    String.joinWith "" $ Array.range minX maxX <#> \x ->
      case M.lookup { x, y } cave of
        Nothing -> "."
        Just Stone -> "#"
        Just Sand -> "o"
  where
  minX = M.keys cave # Array.fromFoldable <#> _.x # minimum # fromMaybe 0
  minY = M.keys cave # Array.fromFoldable <#> _.y # minimum # fromMaybe 0

  maxX = M.keys cave # Array.fromFoldable <#> _.x # maximum # fromMaybe 0
  maxY = M.keys cave # Array.fromFoldable <#> _.y # maximum # fromMaybe 0

stDrawCave :: forall r. STCave r -> ST r String
stDrawCave cave = do
  width <- stCaveWidth cave
  height <- stCaveHeight cave
  for (Array.range 0 (height
  pure ""

stInsertStone :: forall r. Pos -> Pos -> STCave r -> ST r Unit
stInsertStone { x: x1, y: y1 } { x: x2, y: y2 } cave | y1 == y2 = for_ (Array.range x1 x2) $ \x -> stCaveInsert { x, y: y1 } Stone cave
stInsertStone { x: x1, y: y1 } { x: x2, y: y2 } cave | x1 == x2 = for_ (Array.range y1 y2) $ \y -> stCaveInsert { x: x1, y } Stone cave
stInsertStone _ _ _ = pure unit

stInsertPath :: forall r. Path -> STCave r -> ST r Unit
stInsertPath path cave = for_ (windows2 $ List.fromFoldable path)
  \(Tuple from to) -> stInsertStone from to cave

stInsertPaths paths cave = for_ paths \path -> stInsertPath path cave

data STCave r = Grid (STArray r (STArray r (Maybe Block)))

stCaveEmpty :: forall r. ST r (STCave r)
stCaveEmpty = Grid <$> STArray.new

stCaveWidth :: forall r. STCave r -> ST r Int
stCaveWidth (Grid columns) = STArray.length columns

stCaveHeight :: forall r. STCave r -> ST r Int
stCaveHeight (Grid columns) = do
  columns <- STArray.freeze columns
  fromMaybe 0 <$> maximum <$> (for columns $ \column -> STArray.length column)

stCaveLookup :: forall r. Pos -> STCave r -> ST r (Maybe Block)
stCaveLookup { x, y } (Grid columns) = STArray.peek x columns >>= case _ of
    Nothing -> pure Nothing
    Just row -> STArray.peek y row <#> (bindFlipped identity)

stCaveInsert :: forall r. Pos -> Block -> STCave r -> ST r Unit
stCaveInsert { x, y } block (Grid columns) = do
  width <- STArray.length columns
  ST.while (STArray.length columns <#> \width -> width <= x) do
    emptyColumn <- STArray.new
    void $ STArray.push emptyColumn columns
  column <- STArray.peek x columns
  case column of
    Nothing -> pure unit
    Just column -> do
      ST.while (STArray.length column <#> \height -> height < y) do
         void $ STArray.push Nothing column
      void $ STArray.poke y (Just block) column

stSimulateSand :: forall r. Int -> Pos -> STCave r -> ST r Boolean
stSimulateSand floor pos@{ x, y } grid = do
  block <- stCaveLookup pos grid
  case block of
    Nothing -> do
      let downPos = { x, y: y + 1 }
      let leftPos = { x: x - 1, y: y + 1 }
      let rightPos = { x: x + 1, y: y + 1 }
      down <- stCaveLookup downPos grid
      left <- stCaveLookup leftPos grid
      right <- stCaveLookup rightPos grid
      let
        pos =
          ( case down, left, right of
              Nothing, _, _ -> Just downPos
              _, Nothing, _ -> Just leftPos
              _, _, Nothing -> Just rightPos
              _, _, _ -> Nothing
          )
      case pos of
        Just pos -> do
          if y + 1 < floor then do
            stSimulateSand floor pos grid
          else do
            stCaveInsert { x, y } Sand grid
            pure true
        Nothing -> do
          stCaveInsert { x, y } Sand grid
          pure false
    Just _ -> pure false

simulateSand :: Int -> Pos -> State Cave Boolean
simulateSand floor pos@{ x, y } = do
  block <- get <#> M.lookup pos
  case block of
    Nothing -> do
      let downPos = { x, y: y + 1 }
      let leftPos = { x: x - 1, y: y + 1 }
      let rightPos = { x: x + 1, y: y + 1 }
      down <- get <#> M.lookup downPos
      left <- get <#> M.lookup leftPos
      right <- get <#> M.lookup rightPos
      let
        pos =
          ( case down, left, right of
              Nothing, _, _ -> Just downPos
              _, Nothing, _ -> Just leftPos
              _, _, Nothing -> Just rightPos
              _, _, _ -> Nothing
          )
      case pos of
        Just pos -> do
          if y + 1 < floor then do
            simulateSand floor pos
          else do
            modify_ $ M.insert { x, y } Sand
            pure true
        Nothing -> do
          modify_ $ M.insert { x, y } Sand
          pure false
    Just _ -> pure false

stFindAbyss :: forall r. STCave r -> ST r Int
stFindAbyss = stCaveHeight


findAbyss :: Cave -> Int
findAbyss =
  M.toUnfoldableUnordered
    >>> List.filter (snd >>> eq Stone)
    >>> map (fst >>> _.y)
    >>> maximum
    >>> fromMaybe 0

solvePartOne :: forall f. Foldable f => f Path -> String
solvePartOne paths = flip evalState M.empty do
  insertPaths paths
  abyss <- get <#> findAbyss
  count <- tailRecM (go abyss) 0
  get <#> drawCave <#> (_ `append` ("\n" <> show count))
  where
  go abyss n = simulateSand abyss { x: 500, y: 0 }
    <#> case _ of
      true -> Done n
      false -> Loop $ n + 1

solvePartTwo :: forall f. Foldable f => f Path -> Int
solvePartTwo paths = flip evalState M.empty do
  insertPaths paths
  floor <- get <#> findAbyss <#> add 2
  tailRecM (go floor) 0
  where
  go floor n = get <#> M.lookup { x: 500, y: 0 } >>= case _ of
    Just _ -> pure $ Done n
    Nothing -> simulateSand floor { x: 500, y: 0 } $> Loop (n + 1)

stSolvePartTwo :: forall f. Foldable f => f Path -> Int
stSolvePartTwo paths = ST.run do
  cave <- stCaveEmpty
  stInsertPaths paths cave
  floor <- stFindAbyss cave <#> add 2 <#> trace "floor"
  tailRecM (go cave floor) 0
  where
  go :: forall r. STCave r -> Int -> Int -> ST r (Step Int Int)
  go cave floor n = stCaveLookup { x: 500, y: 0 } cave >>= case _ of
    Just _ -> pure $ Done n
    Nothing -> stSimulateSand floor { x: 500, y: 0 } cave $> Loop (n + 1)


partOne = parse >>> map solvePartOne

partTwo = parse >>> map (stSolvePartTwo >>> show)
