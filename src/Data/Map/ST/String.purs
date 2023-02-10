module Data.Map.ST.String
  ( StringMap(..)
  , delete
  , empty
  , entries
  , freeze
  , fromFoldable
  , insert
  , keys
  , lookup
  , values
  ) where

import Prelude

import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as STObject
import Foreign.Object.ST.Extra as STObjectExtra
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

makeVoidST2 :: forall r a b c. (a -> b -> ST r c) -> (a -> b -> ST r Unit)
makeVoidST2 f = unsafeCoerce f

makeVoidST3 :: forall r a b c d. (a -> b -> c -> ST r d) -> (a -> b -> c -> ST r Unit)
makeVoidST3 = unsafeCoerce

newtype StringMap r a = StringMap (STObject r a)

instance newtypeStringMap :: Newtype (StringMap r a) (STObject r a)

empty :: forall r a. ST r (StringMap r a)
empty = coerce $ (STObject.new :: ST r (STObject r a))

insert :: forall r a. String -> a -> StringMap r a -> ST r Unit
insert = makeVoidST3 nonVoid
  where
  nonVoid :: String -> a -> StringMap r a -> ST r (STObject r a)
  nonVoid = coerce (STObject.poke :: String -> a -> STObject r a -> ST r (STObject r a))

delete :: forall r a. String -> StringMap r a -> ST r Unit
delete = makeVoidST2 nonVoid
  where
  nonVoid :: String -> StringMap r a -> ST r (STObject r a)
  nonVoid = coerce (STObject.delete :: String -> STObject r a -> ST r (STObject r a))

lookup :: forall r a. String -> StringMap r a -> ST r (Maybe a)
lookup = coerce (STObject.peek :: String -> STObject r a -> ST r (Maybe a))

entries :: forall r a. StringMap r a -> ST r (Array (Tuple String a))
entries = coerce (STObjectExtra.entries :: STObject r a -> ST r (Array (Tuple String a)))

values :: forall r a. StringMap r a -> ST r (Array a)
values = coerce (STObjectExtra.values :: STObject r a -> ST r (Array a))

keys :: forall r a. StringMap r a -> ST r (Array String)
keys = coerce (STObjectExtra.keys :: STObject r a -> ST r (Array String))

freeze :: forall r a. StringMap r a -> ST r (M.Map String a)
freeze map = M.fromFoldable <$> entries map

fromFoldable :: forall f r a. Traversable f => f (Tuple String a) -> ST r (StringMap r a)
fromFoldable collection = do
  map <- empty
  traverse_ (\(Tuple k v) -> insert k v map) collection
  pure $ map
