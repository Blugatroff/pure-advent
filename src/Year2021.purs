module Year2021 (days) where

import MeLude

import Data.Map as Map
import Day (Day(..), Index(..))

import Year2021.Day1 as Day1
import Year2021.Day2 as Day2
import Year2021.Day3 as Day3
import Year2021.Day4 as Day4
import Year2021.Day5 as Day5
import Year2021.Day6 as Day6
import Year2021.Day7 as Day7

days ∷ Map.Map (Index Day) Day
days = Map.fromFoldable $ map (lmap Index)
  [ 1 /\ Day Day1.partOne Day1.partTwo
  , 2 /\ Day Day2.partOne Day2.partTwo
  , 3 /\ Day Day3.partOne Day3.partTwo
  , 4 /\ Day Day4.partOne Day4.partTwo
  , 5 /\ Day Day5.partOne Day5.partTwo
  , 6 /\ Day Day6.partOne Day6.partTwo
  , 7 /\ Day Day7.partOne Day7.partTwo
  ]
