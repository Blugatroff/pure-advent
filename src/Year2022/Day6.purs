module Year2022.Day6 (day) where

import MeLude

import Data.List as List
import Data.Set as S
import Day (makeDay)
import Util (windows)

allDifferent :: forall a. Ord a => List a -> Boolean
allDifferent list = List.length list == S.size (S.fromFoldable list)

solve :: Int -> String -> Int
solve l stream = stream
  # toCharArray
  # List.fromFoldable
  # windows l
  # List.takeWhile (not <<< allDifferent)
  # List.length
  # (add l)

day = makeDay Right
  (Right <<< show <<< solve 4)
  (Right <<< show <<< solve 14)

