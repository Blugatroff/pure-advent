module Data.Map.ST.Int
  ( IntMap(..)
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
import Data.Either (fromRight)
import Data.Map as M
import Data.Map.ST.String (StringMap)
import Data.Map.ST.String as STMapString
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable, traverse_)
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)
import Util (mapFst, parseInt)

newtype IntMap r a = IntMap (StringMap r a)

empty ∷ forall r a. ST r (IntMap r a)
empty = IntMap <$> STMapString.empty

insert :: forall r a. Int -> a -> IntMap r a -> ST r Unit
insert = unsafeCoerce STMapString.insert

delete :: forall r a. Int -> IntMap r a -> ST r Unit
delete = unsafeCoerce STMapString.delete

lookup :: forall r a. Int -> IntMap r a -> ST r (Maybe a)
lookup = unsafeCoerce STMapString.lookup

entries :: forall r a. IntMap r a -> ST r (Array (Tuple Int a))
entries (IntMap stringMap) = STMapString.entries stringMap <#> (map $ mapFst (fromRight 0 <<< parseInt))

values :: forall r a. IntMap r a -> ST r (Array a)
values (IntMap stringMap) = STMapString.values stringMap

keys :: forall r a. IntMap r a -> ST r (Array Int)
keys (IntMap stringMap) = STMapString.keys stringMap <#> map (fromRight 0 <<< parseInt)

freeze :: forall r a. IntMap r a -> ST r (M.Map Int a)
freeze map = M.fromFoldable <$> entries map

fromFoldable :: forall f r a. Traversable f => f (Tuple Int a) -> ST r (IntMap r a)
fromFoldable collection = do
  map <- empty
  traverse_ (\(Tuple k v) -> insert k v map) collection
  pure $ map
