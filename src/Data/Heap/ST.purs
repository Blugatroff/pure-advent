module Data.Heap.ST where

import Control.Applicative (pure, void)
import Control.Bind (discard)
import Control.Monad (bind, (>>=))
import Control.Monad.ST (ST, while)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Control.Monad.ST.Uncurried (STFn2, mkSTFn2, runSTFn2)
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor (map)
import Data.Int.Bits (shl, shr)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord (class Ord, (<), (>), (<=), (>=))
import Data.Ring (add, (+), (-))
import Data.Unit (Unit, unit)
import Partial.Unsafe (unsafePartial)

data STHeap r a = STHeap
  { values :: STArray r a
  , length :: STRef r Int
  }

empty :: forall r a. ST r (STHeap r a)
empty = do
  values <- STArray.new
  length <- STRef.new 0
  pure $ STHeap { values, length }

insert :: forall r a. Ord a => STHeap r a -> a -> ST r Unit
insert (heap@(STHeap { values, length })) value = do
  _ <- STArray.push value values
  index <- STRef.read length
  _ <- STRef.modify (add 1) length
  up heap index

pop :: forall r a. Ord a => STHeap r a -> ST r (Maybe a)
pop (heap@(STHeap { length: lengthRef, values })) = do
  length <- STRef.read lengthRef
  if length == 0 then pure Nothing
  else do
    top <- runSTFn2 partialArrayPeek 0 values
    bottom <- map (\o -> unsafePartial (Maybe.fromJust o)) (STArray.pop values)
    _ <- STRef.modify (\l -> l - 1) lengthRef
    length <- STRef.read lengthRef
    if length <= 0 then pure unit
    else do
      _ <- STArray.poke 0 bottom values
      down heap 0
    pure $ Just top

partialArrayPeek :: forall r a. STFn2 Int (STArray r a) r a
partialArrayPeek = mkSTFn2 \index array -> do
  o <- STArray.peek index array
  pure $ unsafePartial (Maybe.fromJust o)

up :: forall r a. Ord a => STHeap r a -> Int -> ST r Unit
up (STHeap { values }) pos = do
  item <- runSTFn2 partialArrayPeek pos values
  posRef <- STRef.new pos
  doneRef <- STRef.new false
  let
    condition =
      ( do
          done <- STRef.read doneRef
          if done then pure false
          else do
            pos <- STRef.read posRef
            pure $ pos > 0
      )
  while condition do
    pos <- STRef.read posRef
    let parent = (pos - 1) `shr` 1

    current <- runSTFn2 partialArrayPeek parent values
    let cond = item >= current
    if cond then do
      void $ STRef.write true doneRef
    else do
      void $ STArray.poke pos current values
      void $ STRef.write parent posRef
  pos <- STRef.read posRef
  void $ STArray.poke pos item values

down :: forall r a. Ord a => STHeap r a -> Int -> ST r Unit
down (STHeap { values, length }) pos = do
  length <- STRef.read length
  let halfLength = length `shr` 1
  item <- runSTFn2 partialArrayPeek pos values
  doneRef <- STRef.new false
  posRef <- STRef.new pos
  let
    condition =
      ( do
          done <- STRef.read doneRef
          if done then pure false
          else do
            pos <- STRef.read posRef
            pure $ pos < halfLength
      )
  while condition do
    pos <- STRef.read posRef
    let bestChild = (pos `shl` 1) + 1
    let right = bestChild + 1
    bestChildValue <- runSTFn2 partialArrayPeek bestChild values
    bestChild <- STArray.peek right values >>= case _ of
      Just rightValue | rightValue < bestChildValue -> pure right
      _ -> pure bestChild
    bestChildValue <- runSTFn2 partialArrayPeek bestChild values
    if bestChildValue >= item then do
      void $ STRef.write true doneRef
    else do
      _ <- STArray.poke pos bestChildValue values
      void $ STRef.write bestChild posRef
  pos <- STRef.read posRef
  void $ STArray.poke pos item values
