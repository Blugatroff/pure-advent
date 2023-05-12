-- Source: https://pursuit.purescript.org/packages/purescript-heap/0.1.0
module Data.Heap
  ( Heap
  , Min
  , Max
  , class HeapOrder
  , prefer
  , empty
  , singleton
  , null
  , merge
  , insert
  , root
  , min
  , max
  , deleteRoot
  , deleteMin
  , deleteMax
  , fromFoldable
  , takeMin
  , takeMax
  ) where

import Data.Ord (class Ord, (<), (>))
import Data.Function (flip)
import Data.Functor ((<#>))
import Data.Semiring ((+))
import Type.Proxy (Proxy(Proxy))
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

class HeapOrder t where
  prefer :: forall a. Ord a => t -> a -> a -> Boolean

foreign import data Min :: Type
foreign import data Max :: Type

instance heapOrderMin :: HeapOrder (Proxy Min) where
  prefer _ a b = a < b

instance heapOrderMax :: HeapOrder (Proxy Max) where
  prefer _ a b = a > b


-- | Heap. Implemented using Leftist heap.
data Heap :: forall k. k -> Type -> Type
data Heap t a
  = Empty
  | Node { elem :: a, rank :: Int, left :: Heap t a, right :: Heap t a }

empty :: forall t a. Ord a => Heap t a
empty = Empty

null :: forall t a. Heap t a -> Boolean
null Empty = true
null _ = false

singleton :: forall t a. Ord a => a -> Heap t a
singleton a =
  Node { elem: a, rank: 1, left: Empty, right: Empty }

merge :: forall t a. HeapOrder (Proxy t) => Ord a => Heap t a -> Heap t a -> Heap t a
merge Empty h = h
merge h Empty = h
merge a'@(Node a) b'@(Node b) =
  if prefer (Proxy :: Proxy t) a.elem b.elem then node' a.elem a.left (merge a.right b')
  else node' b.elem b.left (merge b.right a')
  where
  rank Empty = 0
  rank (Node { rank }) = rank

  node' root n0 n1 =
    let
      rank0 = rank n0
      rank1 = rank n1
    in
      if rank0 < rank1 then Node { elem: root, rank: rank0 + 1, left: n1, right: n0 }
      else Node { elem: root, rank: rank1 + 1, left: n0, right: n1 }

insert :: forall t a. HeapOrder (Proxy t) => Ord a => a -> Heap t a -> Heap t a
insert a = merge (singleton a)

root :: forall t a. Heap t a -> Maybe a
root Empty = Nothing
root (Node { elem }) = Just elem

min :: forall a. Heap Min a -> Maybe a
min = root

max :: forall a. Heap Max a -> Maybe a
max = root

deleteRoot :: forall t a. HeapOrder (Proxy t) => Ord a => Heap t a -> Heap t a
deleteRoot Empty = Empty
deleteRoot (Node { left, right }) = merge left right

deleteMin :: forall a. Ord a => Heap Min a -> Heap Min a
deleteMin = deleteRoot

deleteMax :: forall a. Ord a => Heap Max a -> Heap Max a
deleteMax = deleteRoot

fromFoldable :: forall t f a. Ord a => HeapOrder (Proxy t) => Foldable f => f a -> Heap t a
fromFoldable = foldl (flip insert) empty

takeMin :: forall a. Ord a => Heap Min a -> Maybe (a /\ Heap Min a)
takeMin heap = min heap <#> \a -> a /\ deleteMin heap

takeMax :: forall a. Ord a => Heap Max a -> Maybe (a /\ Heap Max a)
takeMax heap = max heap <#> \a -> a /\ deleteMax heap

