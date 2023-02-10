module Year2022.Day6 (partOne, partTwo) where

import Prelude

import Data.Either (Either(..))
import Data.List (List, fromFoldable, length, takeWhile)
import Data.Set as S
import Data.String.CodeUnits (toCharArray)
import Effect.Exception (Error)
import Util (windows)

allDifferent :: forall a. Ord a => List a -> Boolean
allDifferent list = length list == S.size (S.fromFoldable list)

solve :: Int -> String -> Int
solve l stream = stream
  # toCharArray
  # fromFoldable
  # windows l
  # takeWhile (not <<< allDifferent)
  # length
  # (add l)

partOne :: String -> Either Error String
partOne input = solve 4 input # show # Right

partTwo :: String -> Either Error String
partTwo input = solve 14 input # show # Right
