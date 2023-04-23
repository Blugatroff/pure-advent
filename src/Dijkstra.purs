module Dijkstra where

import MeLude

import Control.Monad.Rec.Class (Step(Done, Loop), tailRecM)
import Control.Monad.Rec.Class (tailRecM)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Control.Monad.ST.STFn as STFn
import Control.Monad.ST.Uncurried (STFn2, runSTFn1, runSTFn2, mkSTFn2)
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Function.Uncurried (Fn3, mkFn3, runFn3)
import Data.List as List
import Data.Primitive (class Primitive, class PrimitiveKey, primitiveKey)
import Data.PriorityQueue as PQ
import Data.Set.Native.ST (NativeSetST)
import Data.Set.Native.ST as NativeSetST
import Data.Traversable (foldr)
import Data.Tuple (Tuple(..))
import Debug (traceM)

class PrimitiveKey pos primitive <= World world pos primitive | pos -> primitive where
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

data Path pos = PathEnd (Maybe (Lazy (Path pos))) pos Int Int | PathBranch (Maybe (Lazy (Path pos))) pos Int (Array (Lazy (Path pos)))

instance Show pos => Show (Path pos) where
  show (PathEnd _ pos cost cellCost) = "(PathEnd " <> show pos <> " " <> show cost <> " " <> show cellCost <> ")"
  show (PathBranch _ pos cost _) = "(PathBranch " <> show pos <> " " <> show cost <> ")"

instance (Show pos, Eq pos) => Eq (Path pos) where
  eq (PathBranch _ pos1 cost1 _) (PathBranch _ pos2 cost2 _) = pos1 == pos2 && cost1 == cost2 -- && parent1 == parent2
  eq (PathEnd _ pos1 cost1 cellCost1) (PathEnd _ pos2 cost2 cellCost2) = pos1 == pos2 && cost1 == cost2 && cellCost1 == cellCost2 -- && parent1 == parent2
  eq _ _ = false

instance (Show pos, Eq pos) => Ord (Path pos) where
  compare (PathEnd _ _ cost1 _) (PathEnd _ _ cost2 _) = compare cost1 cost2
  compare (PathBranch _ _ cost1 _) (PathBranch _ _ cost2 _) = compare cost1 cost2
  compare (PathBranch _ _ cost1 _) (PathEnd _ _ cost2 _) = compare cost1 cost2
  compare (PathEnd _ _ cost1 _) (PathBranch _ _ cost2 _) = compare cost1 cost2

findPaths :: forall world pos primitive. Show pos => World world pos primitive => Fn3 (Maybe (Lazy (Path pos))) (Tuple pos Int) world (Maybe (Lazy (Path pos)))
findPaths = inner
  where
  inner = mkFn3 \previous state world ->
    let
      (pos /\ cost) = state
    in
      case lookupCell pos world of
        Nothing -> Nothing
        Just cell ->
          let
            this = defer \_ -> case cell of
              Destination _ -> PathEnd previous pos cost (cellCost cell)
              _ ->
                PathBranch previous pos cost
                  $ Array.mapMaybe (\p -> runFn3 inner (Just this) (Tuple p (cost + (cellCost cell))) world)
                  $ adjacentCells pos world
          in
            Just this

type PathQueue pos = PQ.PriorityQueue (Path pos)

type Visited r = NativeSetST r

evaluateNextBranch :: forall r pos t. PrimitiveKey pos t => Show pos => Ord pos => PathQueue pos -> Visited r t -> ST r (Maybe (Solution pos))
evaluateNextBranch queue visited = do
  queueRef <- STRef.new queue
  solutionRef <- STRef.new Nothing
  runSTFn2 STFn.while (STRef.read solutionRef <#> Maybe.isNothing) do
    queue <- runSTFn1 STFn.read queueRef
    case PQ.takeMin queue of
        Nothing -> pure unit
        Just (PathEnd p pos cost cellCost /\ _) -> do
          let solution = Just { path: List.reverse $ pos : maybe List.Nil buildPath (force <$> p), cost: cost + cellCost }
          runSTFn2 STFn.write solution solutionRef
        Just (PathBranch _ pos _ next /\ remainingQueue) -> do
          visitedBefore <- runSTFn2 NativeSetST.memberSTFn (primitiveKey pos) visited
          runSTFn2 STFn.write remainingQueue queueRef
          if visitedBefore then pure unit
          else do
            runSTFn2 NativeSetST.insertSTFn (primitiveKey pos) visited
            runSTFn2 STFn.modify (\queue -> foldr PQ.insert queue (force <$> next)) queueRef
  STRef.read solutionRef

buildPath :: forall pos. Path pos -> List pos
buildPath (PathEnd previous pos _ _) = pos : maybe List.Nil buildPath (force <$> previous)
buildPath (PathBranch previous pos _ _) = pos : maybe List.Nil buildPath (force <$> previous)

findSolutionFrom :: forall pos world primitive. Show pos => Ord pos => World world pos primitive => world -> pos -> Maybe (Solution pos)
findSolutionFrom world pos = ST.run do
  case runFn3 findPaths Nothing (Tuple pos 0) world of
    Nothing -> pure Nothing
    Just root -> do
      visited <- NativeSetST.empty
      evaluateNextBranch (PQ.singleton $ force root) visited

