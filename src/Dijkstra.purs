module Dijkstra where

import Prelude

import Data.Array as Array
import Data.Lazy (Lazy, defer, force)
import Data.List (List, (:))
import Data.List as List
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Set as S
import Data.Traversable (foldr)
import Data.Tuple (Tuple(..))
import PriorityQueue as PQ
import Util (trace)

class World world pos where
  lookupCell :: pos -> world -> Maybe Cell
  adjacentCells :: pos -> world -> Array pos

data Cell = Destination Int | Cell Int

instance showCell :: Show Cell where
  show (Cell n) = "(Cell " <> show n <> ")"
  show (Destination n) = "(Destination " <> show n <> ")"

type Solution pos = { path :: List pos, cost :: Int }

cellCost :: Cell -> Int
cellCost (Destination cost) = cost
cellCost (Cell cost) = cost

data Path pos = PathEnd (Maybe (Path pos)) pos Int Int | PathBranch (Maybe (Path pos)) pos Int (Array (Lazy (Path pos)))

instance Show pos => Show (Path pos) where
  show (PathEnd _ pos cost cellCost) = "(PathEnd " <> show pos <> " " <> show cost <> " " <> show cellCost <> ")"
  show (PathBranch _ pos cost _) = "(PathBranch " <> show pos <> " " <> show cost <> ")"

instance (Show pos, Eq pos) => Eq (Path pos) where
  eq (PathBranch parent1 pos1 cost1 _) (PathBranch parent2 pos2 cost2 _) = pos1 == pos2 && cost1 == cost2 -- && parent1 == parent2
  eq (PathEnd parent1 pos1 cost1 cellCost1) (PathEnd parent2 pos2 cost2 cellCost2) = pos1 == pos2 && cost1 == cost2 && cellCost1 == cellCost2 -- && parent1 == parent2
  eq _ _ = false

instance (Show pos, Eq pos) => Ord (Path pos) where
  compare (PathEnd _ _ cost1 _) (PathEnd _ _ cost2 _) = compare cost1 cost2
  compare (PathBranch _ _ cost1 _) (PathBranch _ _ cost2 _) = compare cost1 cost2
  compare (PathBranch _ _ cost1 _) (PathEnd _ _ cost2 _) = compare cost1 cost2
  compare (PathEnd _ _ cost1 _) (PathBranch _ _ cost2 _) = compare cost1 cost2

findPaths :: forall world pos. World world pos => Maybe (Lazy (Path pos)) -> Tuple pos Int -> world -> Maybe (Lazy (Path pos))
findPaths previous (Tuple pos cost) world =
  lookupCell pos world <#> f
  where
  f cell = this
    where
    thisCost = cellCost cell

    next :: Lazy (Array (Lazy (Path pos)))
    next = defer $ \_ -> adjacentCells pos world
      <#> (\p -> findPaths (Just this) (Tuple p (cost + thisCost)) world)
      # Array.catMaybes

    this :: Lazy (Path pos)
    this = defer $ \_ -> case cell of
      Destination _ -> PathEnd (force <$> previous) pos cost thisCost
      _ -> PathBranch (force <$> previous) pos cost $ force next

type PathQueue pos = PQ.PriorityQueue (Path pos)

type Visited pos = S.Set pos

evaluateNextBranch :: forall pos. Show pos => Ord pos => PathQueue pos -> Visited pos -> Lazy (Maybe (Solution pos))
evaluateNextBranch queue visited = case PQ.viewMin queue of
  Nothing -> defer \_ -> Nothing
  Just (PathEnd p pos cost cellCost) -> defer \_ -> Just { path: List.reverse $ pos : maybe List.Nil buildPath p, cost: cost + cellCost }
  Just (path@(PathBranch _ pos _ next)) ->
    let
      remainingQueue = PQ.delete path queue
    in
      if S.member pos visited then evaluateNextBranch remainingQueue visited
      else evaluateNextBranch (foldr PQ.insert remainingQueue (force <$> next)) (S.insert pos visited)

buildPath :: forall pos. Path pos -> List pos
buildPath (PathEnd previous pos _ _) = pos : maybe List.Nil buildPath previous
buildPath (PathBranch previous pos _ _) = pos : maybe List.Nil buildPath previous

findSolutionFrom :: forall pos world. Show pos => Ord pos => World world pos => world -> pos -> Maybe (Solution pos)
findSolutionFrom world pos = do
  root <- findPaths Nothing (Tuple pos 0) world
  force $ evaluateNextBranch (PQ.singleton $ force root) S.empty
