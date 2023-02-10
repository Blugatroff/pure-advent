module Data.Map.ST.String
  ( StringMap
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
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as STObject
import Foreign.Object.ST.Extra as STObjectExtra

data StringMap r a = StringMap (STObject r a)

empty :: forall r a. ST r (StringMap r a)
empty = StringMap <$> STObject.new

insert :: forall r a. String -> a -> StringMap r a -> ST r Unit
insert k v (StringMap obj) = void $ STObject.poke k v obj

delete :: forall r a. String -> StringMap r a -> ST r Unit
delete k (StringMap obj) = void $ STObject.delete k obj

lookup :: forall r a. String -> StringMap r a -> ST r (Maybe a)
lookup k (StringMap obj) = STObject.peek k obj

entries :: forall r a. StringMap r a -> ST r (Array (Tuple String a))
entries (StringMap obj) = STObjectExtra.entries obj

values :: forall r a. StringMap r a -> ST r (Array a)
values (StringMap obj) = STObjectExtra.values obj

keys :: forall r a. StringMap r a -> ST r (Array String)
keys (StringMap obj) = STObjectExtra.keys obj

freeze :: forall r a. StringMap r a -> ST r (M.Map String a)
freeze map = M.fromFoldable <$> entries map

fromFoldable :: forall f r a. Traversable f => f (Tuple String a) -> ST r (StringMap r a)
fromFoldable collection = do
  map <- empty
  traverse_ (\(Tuple k v) -> insert k v map) collection
  pure $ map
