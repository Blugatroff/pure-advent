module Data.Map.Native.ST (empty, memberSTFn, insertSTFn, removeSTFn, member, lookup, lookupSTFn, insert, remove, entries, size, toUnfoldable, fromFoldable, NativeMapST) where

import Control.Monad.ST (ST, Region)
import Control.Monad.ST.Uncurried (STFn1, STFn2, STFn3, runSTFn1, runSTFn2, runSTFn3)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn1, Fn2, mkFn2)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Primitive (class Primitive)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Unfoldable (class Unfoldable)
import Data.Unit (Unit)

foreign import data NativeMapST :: Region -> Type -> Type -> Type

foreign import empty :: forall r k v. ST r (NativeMapST r k v)

foreign import memberSTFnImpl :: forall r k v. STFn2 k (NativeMapST r k v) r Boolean

foreign import lookupSTFnImpl :: forall r k v. (forall a. Maybe a) -> (forall a. a -> Maybe a) -> STFn2 k (NativeMapST r k v) r (Maybe v)

foreign import insertSTFnImpl :: forall r k v. STFn3 k v (NativeMapST r k v) r Unit

foreign import removeSTFnImpl :: forall r k v. STFn2 k (NativeMapST r k v) r Unit

foreign import entriesSTFnImpl :: forall r k v. (Fn2 k v (k /\ v)) -> STFn1 (NativeMapST r k v) r (Array (k /\ v))

foreign import sizeSTFn :: forall r k v. STFn1 (NativeMapST r k v) r Int

foreign import fromEntriesImpl :: forall r k v. (Fn1 (k /\ v) k) -> (Fn1 (k /\ v) v) -> STFn1 (Array (k /\ v)) r (NativeMapST r k v)

memberSTFn :: forall r k v. Primitive k => STFn2 k (NativeMapST r k v) r Boolean
memberSTFn = memberSTFnImpl

lookupSTFn :: forall r k v. Primitive k => STFn2 k (NativeMapST r k v) r (Maybe v)
lookupSTFn = lookupSTFnImpl Nothing Just

insertSTFn :: forall r k v. Primitive k => STFn3 k v (NativeMapST r k v) r Unit
insertSTFn = insertSTFnImpl

removeSTFn :: forall r k v. Primitive k => STFn2 k (NativeMapST r k v) r Unit
removeSTFn = removeSTFnImpl

fromEntriesSTFn = fromEntriesImpl fst snd

entriesSTFn = entriesSTFnImpl (mkFn2 \a b -> a /\ b)

member = runSTFn2 memberSTFn
lookup = runSTFn2 lookupSTFn
insert = runSTFn3 insertSTFn
remove = runSTFn2 removeSTFn
entries = runSTFn1 entriesSTFn
size = runSTFn1 sizeSTFn
fromEntries = runSTFn1 fromEntriesSTFn

toUnfoldable :: forall f r k v. Unfoldable f => NativeMapST r k v -> ST r (f (k /\ v))
toUnfoldable set = map Array.toUnfoldable (entries set)

fromFoldable :: forall f r k v. Foldable f => f (k /\ v) -> ST r (NativeMapST r k v)
fromFoldable f = fromEntries (Array.fromFoldable f)

