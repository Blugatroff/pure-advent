module Year2022.Day14 (partOne, partTwo) where

import MeLude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.List as List
import Data.Map.ST.Int (IntMap)
import Data.Map.ST.Int as IntMap
import Data.String as String
import Data.Traversable (for, minimum)
import Util (lines, parseInt, splitStringOnce, windows2)

type Pos = { x :: Int, y :: Int }

type Path = Array Pos

data Block = Stone | Sand

instance showBlock :: Show Block where
  show Stone = "Stone"
  show Sand = "Sand"

derive instance eqBlock :: Eq Block

parsePath ∷ String → Either String (Array Pos)
parsePath line =
  String.split (String.Pattern "->") line
    <#> String.trim
    <#> splitStringOnce ","
    # traverse case _ of
        Just (x /\ y) -> do
          x <- parseInt x
          y <- parseInt y
          Right { x, y }
        Nothing -> Left $ "failed to parse path: " <> line

parse ∷ String → Either String (Array (Array Pos))
parse input = lines input <#> String.trim # Array.filter (not <<< String.null) # traverse parsePath

data Cave r = Cave (IntMap r (IntMap r Block))

caveEmpty :: forall r. ST r (Cave r)
caveEmpty = Cave <$> IntMap.empty

caveLookup :: forall r. Pos -> Cave r -> ST r (Maybe Block)
caveLookup { x, y } (Cave columns) = IntMap.lookup x columns >>= case _ of
  Just column -> IntMap.lookup y column
  Nothing -> pure Nothing

caveInsert :: forall r. Pos -> Block -> Cave r -> ST r Unit
caveInsert { x, y } block (Cave columns) = do
  IntMap.lookup x columns >>= case _ of
    Just column -> void $ IntMap.insert y block column
    Nothing -> do
      column <- IntMap.empty
      void $ IntMap.insert y block column
      void $ IntMap.insert x column columns

caveEntries :: forall r. Cave r -> ST r (Array (Pos /\ Block))
caveEntries (Cave columns) = Array.concat <$> do
  columns <- IntMap.entries columns
  for columns \(x /\ column) -> do
    cells <- IntMap.entries column
    pure $ (\(y /\ block) -> { x, y } /\ block) <$> cells

insertStone :: forall r. Pos -> Pos -> Cave r -> ST r Unit
insertStone { x: x1, y: y1 } { x: x2, y: y2 } cave | y1 == y2 = for_ (Array.range x1 x2) $ \x -> caveInsert { x, y: y1 } Stone cave
insertStone { x: x1, y: y1 } { x: x2, y: y2 } cave | x1 == x2 = for_ (Array.range y1 y2) $ \y -> caveInsert { x: x1, y } Stone cave
insertStone _ _ _ = pure unit

insertPath :: forall r. Path -> Cave r -> ST r Unit
insertPath path cave = for_ (windows2 $ List.fromFoldable path)
  \(from /\ to) -> insertStone from to cave

insertPaths ∷ forall f r. Foldable f => f (Array Pos) → Cave r → ST r Unit
insertPaths paths cave = for_ paths \path -> insertPath path cave

simulateSand :: forall r. Int -> Pos -> Cave r -> ST r Boolean
simulateSand floor pos cave = tailRecM go pos
  where
  go :: Pos -> ST r (Step Pos Boolean)
  go pos@{ x, y } = caveLookup pos cave >>= case _ of
    Nothing -> choose pos >>= case _ of
      Just pos -> do
        if y + 1 < floor then do
          pure $ Loop pos
        else do
          caveInsert { x, y } Sand cave
          pure $ Done true
      Nothing -> do
        caveInsert { x, y } Sand cave
        pure $ Done false
    Just _ -> pure $ Done false

  choose :: Pos -> ST r (Maybe Pos)
  choose { x, y } = do
    let downPos = { x, y: y + 1 }
    let leftPos = { x: x - 1, y: y + 1 }
    let rightPos = { x: x + 1, y: y + 1 }
    down <- caveLookup downPos cave
    left <- caveLookup leftPos cave
    right <- caveLookup rightPos cave
    pure $ case down, left, right of
      Nothing, _, _ -> Just downPos
      _, Nothing, _ -> Just leftPos
      _, _, Nothing -> Just rightPos
      _, _, _ -> Nothing

findAbyss :: forall r. Cave r -> ST r Int
findAbyss cave = caveEntries cave
  <#> Array.filter (snd >>> eq Stone)
  <#> map (fst >>> _.y)
  <#> maximum
  <#> fromMaybe 0

drawCave :: forall r. Cave r -> ST r String
drawCave cave = do
  minX <- findDimension _.x minimum
  minY <- findDimension _.y minimum
  maxX <- findDimension _.x maximum
  maxY <- findDimension _.y maximum
  String.joinWith "\n" <$>
    for (Array.range minY maxY) \y ->
      String.joinWith "" <$> for (Array.range minX maxX) \x -> do
        block <- caveLookup { x, y } cave
        pure $ case block of
          Nothing -> "."
          Just Sand -> "o"
          Just Stone -> "#"
  where
  findDimension key extreme = caveEntries cave
    >>= \entries -> pure $ fromMaybe 0 $ extreme $ key <$> fst <$> entries

solvePartOne :: forall f. Foldable f => f Path -> String
solvePartOne paths = ST.run do
  cave <- caveEmpty
  insertPaths paths cave
  abyss <- findAbyss cave
  count <- tailRecM (go cave abyss) 0
  drawCave cave <#> \drawing -> drawing <> "\n" <> show count
  where
  go :: forall r. Cave r -> Int -> Int -> ST r (Step Int Int)
  go cave abyss n = simulateSand abyss { x: 500, y: 0 } cave
    <#> case _ of
      true -> Done n
      false -> Loop $ n + 1

solvePartTwo :: forall f. Foldable f => f Path -> Int
solvePartTwo paths = ST.run do
  cave <- caveEmpty
  insertPaths paths cave
  floor <- findAbyss cave <#> add 2
  tailRecM (go cave floor) 0
  where
  go :: forall r. Cave r -> Int -> Int -> ST r (Step Int Int)
  go cave floor n = caveLookup { x: 500, y: 0 } cave >>= case _ of
    Just _ -> pure $ Done n
    Nothing -> simulateSand floor { x: 500, y: 0 } cave $> Loop (n + 1)

partOne ∷ String → Either String String
partOne = parse >>> map solvePartOne

partTwo ∷ String → Either String String
partTwo = parse >>> map solvePartTwo >>> map show
