module Dijkstra (findPath, Cell(..), class World, lookupCell, adjacentCells) where

import MeLude

import Control.Monad.Rec.Class (tailRecM, Step(..))
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Control.Monad.ST.Uncurried (runSTFn2, runSTFn3)
import Data.Array as Array
import Data.Heap as Heap
import Data.List as List
import Data.Map as M
import Data.Map.Native.ST as NativeMapST
import Data.Maybe (isNothing)
import Data.Primitive (class PrimitiveKey, primitiveKey)
import Data.Set.Native.ST as NativeSetST
import Partial.Unsafe (unsafePartial)

class PrimitiveKey pos primitive <= World world pos primitive | pos -> primitive where
  lookupCell :: pos -> world -> Maybe Cell
  adjacentCells :: pos -> world -> Array pos

data Cell = Destination Int | Cell Int

instance eqCell :: Eq Cell where
  eq (Cell a) (Cell b) = eq a b
  eq (Destination a) (Destination b) = eq a b
  eq _ _ = false

instance showCell :: Show Cell where
  show (Cell n) = "(Cell " <> show n <> ")"
  show (Destination n) = "(Destination " <> show n <> ")"

type Solution pos = { path :: List pos, cost :: Int }

cellCost :: Cell -> Int
cellCost (Destination cost) = cost
cellCost (Cell cost) = cost

type Graph pos = Map pos (Cell /\ List { pos :: pos, cell :: Cell })

data Distance a = Distance a | Infinity

infinityToNothing :: forall a. Distance a -> Maybe a
infinityToNothing Infinity = Nothing
infinityToNothing (Distance a) = Just a

nothingToInfinity :: forall a. Maybe (Distance a) -> Distance a
nothingToInfinity Nothing = Infinity
nothingToInfinity (Just d) = d

instance eqDistance :: Eq a => Eq (Distance a) where
  eq Infinity Infinity = true
  eq (Distance a) (Distance b) = eq a b
  eq _ _ = false

instance ordDistance :: Ord a => Ord (Distance a) where
  compare Infinity Infinity = EQ
  compare (Distance _) Infinity = LT
  compare Infinity (Distance _) = GT
  compare (Distance a) (Distance b) = compare a b

instance showDistance :: Show a => Show (Distance a) where
  show Infinity = "Infinity"
  show (Distance a) = show a

instance semiringDistance :: (Eq a, Semiring a) => Semiring (Distance a) where
  zero = Distance zero
  one = Distance one
  add Infinity _ = Infinity
  add _ Infinity = Infinity
  add (Distance a) (Distance b) = Distance (add a b)
  mul Infinity a = if a == zero then zero else Infinity
  mul a Infinity = mul Infinity a
  mul (Distance a) (Distance b) = Distance (mul a b)

lookupDistance :: forall k d. Ord k => Map k (Distance d) -> k -> Distance d
lookupDistance m k = fromMaybe Infinity $ M.lookup k m

infix 10 lookupDistance as !??

data OrdFst k v = OrdFst k v

instance eqOrdFst :: (Eq k, Eq v) => Eq (OrdFst k v) where
  eq (OrdFst k1 v1) (OrdFst k2 v2) = k1 == k2 && v1 == v2

instance ordOrdFst :: (Eq k, Eq v, Ord k) => Ord (OrdFst k v) where
  compare (OrdFst k1 _) (OrdFst k2 _) = compare k1 k2

findPath
  :: forall world pos primitive
   . World world pos primitive
  => PrimitiveKey pos primitive
  => Ord pos
  => world
  -> pos
  -> Maybe { cost :: Int, path :: List pos }
findPath world src = do
  srcCell <- lookupCell src world

  ST.run do
    visited <- NativeSetST.empty
    distances <- NativeMapST.fromFoldable [primitiveKey src /\ Distance 0 ]
    queueRef <- STRef.new $ (Heap.fromFoldable [ OrdFst (Distance 0) { pos: src, cell: srcCell } ] :: Heap.Heap Heap.Min _)
    prevs <- NativeMapST.empty
    endPosRef <- STRef.new Nothing

    ST.while (isNothing <$> STRef.read endPosRef) do
      queue <- STRef.read queueRef
      let min = Heap.min queue
      void $ STRef.modify Heap.deleteMin queueRef
      case min of
        Nothing -> do
          void $ STRef.write (Just Nothing) endPosRef
        Just (OrdFst _ { pos, cell }) -> case cell of
          Destination _ -> do
            void $ STRef.write (Just (Just pos)) endPosRef
          Cell _ -> do
            alreadyVisited <- runSTFn2 NativeSetST.memberSTFn (primitiveKey pos) visited
            if alreadyVisited then do
              void $ STRef.write queue queueRef
            else do
              let adjacents = adjacentCells pos world
              let unvisitedNeighbours = Array.mapMaybe (\pos -> lookupCell pos world <#> \cell -> { pos, cell }) adjacents
              ST.foreach unvisitedNeighbours \neighbour -> do
                alreadyVisited <- runSTFn2 NativeSetST.memberSTFn (primitiveKey neighbour.pos) visited
                if alreadyVisited then pure unit
                else do
                  posDistance <- map nothingToInfinity $ NativeMapST.lookup (primitiveKey pos) distances
                  neighbourDistance <- map nothingToInfinity $ NativeMapST.lookup (primitiveKey neighbour.pos) distances
                  let altDistance = posDistance + (Distance (cellCost neighbour.cell))
                  if altDistance >= neighbourDistance then pure unit
                  else do
                    runSTFn3 NativeMapST.insertSTFn (primitiveKey neighbour.pos) altDistance distances
                    void $ STRef.modify (Heap.insert (OrdFst altDistance neighbour)) queueRef
                    runSTFn3 NativeMapST.insertSTFn (primitiveKey neighbour.pos) pos prevs
    endPos <- STRef.read endPosRef
    unsafePartial $ case endPos of
      Just Nothing -> pure Nothing
      Just (Just endPos) -> do
        path <- List.reverse <$> flip tailRecM { pos: endPos, path: List.Nil } \{ pos, path } -> do
          v <- NativeMapST.lookup (primitiveKey pos) prevs
          case v of
            Nothing -> pure $ Done path
            Just prev -> pure $ Loop { pos: prev, path: (prev : path) }

        endPosDistance <- (infinityToNothing =<< _) <$> NativeMapST.lookup (primitiveKey endPos) distances
        pure $ endPosDistance <#> \cost -> { cost, path }

