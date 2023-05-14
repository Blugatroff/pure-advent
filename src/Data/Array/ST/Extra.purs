module Data.Array.ST.Extra (findIndex) where

import Control.Monad.ST.Uncurried (STFn2, runSTFn2)
import Data.Maybe (Maybe(..))
import Data.Array.ST (STArray)


foreign import findIndexImpl :: forall r a. (forall x. Maybe x) -> (forall x. x -> Maybe x) -> STFn2 (STArray r a) (a -> Boolean) r (Maybe Int)

findIndexSTFn = findIndexImpl Nothing Just
findIndex = runSTFn2 findIndexSTFn

