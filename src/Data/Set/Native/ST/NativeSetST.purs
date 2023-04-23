module Data.Set.Native.ST (empty, memberSTFn, insertSTFn, removeSTFn, member, insert, remove, entries, size, toUnfoldable, fromFoldable, NativeSetST) where

import Data.Functor (map)
import Data.Array as Array
import Data.Unit (Unit)
import Data.Unfoldable (class Unfoldable)
import Data.Foldable (class Foldable)
import Data.Primitive (class Primitive)
import Control.Monad.ST (ST, Region)
import Control.Monad.ST.Uncurried (STFn1, STFn2, runSTFn1, runSTFn2)

foreign import data NativeSetST :: Region -> Type -> Type

foreign import empty :: forall r a. ST r (NativeSetST r a)

foreign import memberSTFnImpl :: forall r a. STFn2 a (NativeSetST r a) r Boolean

foreign import insertSTFnImpl :: forall r a. STFn2 a (NativeSetST r a) r Unit

foreign import removeSTFnImpl :: forall r a. STFn2 a (NativeSetST r a) r Unit

foreign import entriesSTFn :: forall r a. STFn1 (NativeSetST r a) r (Array a)

foreign import sizeSTFn :: forall r a. STFn1 (NativeSetST r a) r Int

foreign import fromEntries :: forall r a. STFn1 (Array a) r (NativeSetST r a)


memberSTFn :: forall r a. Primitive a => STFn2 a (NativeSetST r a) r Boolean
memberSTFn = memberSTFnImpl

insertSTFn :: forall r a. Primitive a => STFn2 a (NativeSetST r a) r Unit
insertSTFn = insertSTFnImpl

removeSTFn :: forall r a. Primitive a => STFn2 a (NativeSetST r a) r Unit
removeSTFn = removeSTFnImpl

member = runSTFn2 memberSTFn
insert = runSTFn2 insertSTFn
remove = runSTFn2 removeSTFn
entries = runSTFn1 entriesSTFn
size = runSTFn1 sizeSTFn

toUnfoldable :: forall f r a. Unfoldable f => NativeSetST r a -> ST r (f a)
toUnfoldable set = map Array.toUnfoldable (entries set)

fromFoldable :: forall f r a. Foldable f => f a -> ST r (NativeSetST r a)
fromFoldable f = runSTFn1 fromEntries (Array.fromFoldable f)

