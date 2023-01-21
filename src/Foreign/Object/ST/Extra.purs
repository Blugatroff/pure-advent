module Foreign.Object.ST.Extra
  ( entries
  , keys
  , values
  ) where

import Control.Monad.ST (ST)
import Data.Tuple (Tuple(..))
import Foreign.Object.ST (STObject)

foreign import entriesImpl
  :: forall region a
   . (forall l r. l -> r -> Tuple l r)
  -> STObject region a
  -> ST region (Array (Tuple String a))

entries ∷ forall r a. STObject r a → ST r (Array (Tuple String a))
entries = entriesImpl Tuple

foreign import values :: forall r a. STObject r a -> ST r (Array a)

foreign import keys :: forall r a. STObject r a -> ST r (Array String)
