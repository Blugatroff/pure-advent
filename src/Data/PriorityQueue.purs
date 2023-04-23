module Data.PriorityQueue
  ( PriorityQueue
  , delete
  , insert
  , null
  , singleton
  , size
  , takeMax
  , takeMin
  , viewMax
  , viewMin
  , empty
  ) where

import Prelude

import Data.List (List, (:))
import Data.List as List
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (foldl, maximum, minimum)
import Data.Tuple (Tuple(..))

data PriorityQueue k = PriorityQueue (M.Map k (List k))

instance showPriorityQueue :: Show k => Show (PriorityQueue k) where
  show (PriorityQueue m) = show m

empty :: forall k. PriorityQueue k
empty = PriorityQueue M.empty

singleton :: forall k. k -> PriorityQueue k
singleton k = PriorityQueue $ M.singleton k $ List.singleton k

insert :: forall k. Ord k => k -> PriorityQueue k -> PriorityQueue k
insert k (PriorityQueue m) = PriorityQueue $ M.alter alter k m
  where
  alter Nothing = Just $ List.singleton k
  alter (Just ks) = Just $ k : ks

size :: forall k. PriorityQueue k -> Int
size (PriorityQueue m) = foldl (\acc ks -> List.length ks + acc) 0 m

delete :: forall k. Ord k => k -> PriorityQueue k -> PriorityQueue k
delete k (PriorityQueue m) = PriorityQueue $ M.alter alter k m
  where
  alter Nothing = Nothing
  alter (Just ks) = case List.filter (notEq k) ks of
    List.Nil -> Nothing
    ks -> Just ks

viewMin :: forall k. Ord k => PriorityQueue k -> Maybe k
viewMin (PriorityQueue m) = M.findMin m <#> _.value >>= minimum

viewMax :: forall k. Ord k => PriorityQueue k -> Maybe k
viewMax (PriorityQueue m) = M.findMax m <#> _.value >>= maximum

takeMin :: forall k. Ord k => PriorityQueue k -> Maybe (Tuple k (PriorityQueue k))
takeMin queue = viewMin queue <#> \k -> Tuple k (delete k queue)

takeMax :: forall k. Ord k => PriorityQueue k -> Maybe (Tuple k (PriorityQueue k))
takeMax queue = viewMax queue <#> \k -> Tuple k (delete k queue)

null :: forall k. PriorityQueue k -> Boolean
null (PriorityQueue m) = M.size m == 0
