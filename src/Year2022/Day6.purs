module Year2022.Day6 (partOne, partTwo) where

import MeLude

import Data.List as List
import Data.Set as S
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

partOne :: String -> String |? String
partOne input = solve 4 input # show # Right

partTwo :: String -> String |? String
partTwo input = solve 14 input # show # Right
