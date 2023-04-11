module Year2021 (days) where

import MeLude

import Data.Map as Map
import Day (Day(..), Index(..))

import Year2021.Day1 as Day1

days ∷ Map.Map (Index Day) Day
days = Map.fromFoldable $ map (lmap Index)
  [ 1 /\ Day Day1.partOne Day1.partTwo
  ]
