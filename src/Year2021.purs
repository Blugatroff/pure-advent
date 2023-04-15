module Year2021 (days) where

import MeLude

import Data.Map as Map
import Day (Day(..), Index(..))

import Year2021.Day1 as Day1
import Year2021.Day2 as Day2
import Year2021.Day3 as Day3

days ∷ Map.Map (Index Day) Day
days = Map.fromFoldable $ map (lmap Index)
  [ 1 /\ Day Day1.partOne Day1.partTwo
  , 2 /\ Day Day2.partOne Day2.partTwo
  , 3 /\ Day Day3.partOne Day3.partTwo
  ]
