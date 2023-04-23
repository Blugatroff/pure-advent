module Control.Monad.ST.STFn (read, write, modify, while) where

import Data.Unit (Unit)
import Control.Monad.ST (ST)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Uncurried (STFn1, STFn2)

foreign import read :: forall a r. STFn1 (STRef r a) r a

foreign import whileImpl :: forall r. STFn2 (ST r Boolean) (ST r Unit) r Unit
while = whileImpl

foreign import write :: forall a r. STFn2 a (STRef r a) r Unit

foreign import modify :: forall a r. STFn2 (a -> a) (STRef r a) r Unit

