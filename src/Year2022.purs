module Year2022 (days) where

import MeLude

import Data.Map as Map
import Day (Day(..), Index(..))

import Year2022.Day1 as Day1
import Year2022.Day10 as Day10
import Year2022.Day11 as Day11
import Year2022.Day12 as Day12
import Year2022.Day13 as Day13
import Year2022.Day14 as Day14
import Year2022.Day15 as Day15
import Year2022.Day16 as Day16
import Year2022.Day17 as Day17
import Year2022.Day18 as Day18
import Year2022.Day19 as Day19
import Year2022.Day20 as Day20
import Year2022.Day21 as Day21
import Year2022.Day22 as Day22
import Year2022.Day2 as Day2
import Year2022.Day3 as Day3
import Year2022.Day4 as Day4
import Year2022.Day5 as Day5
import Year2022.Day6 as Day6
import Year2022.Day7 as Day7
import Year2022.Day8 as Day8
import Year2022.Day9 as Day9

days ∷ Map.Map (Index Day) Day
days = Map.fromFoldable $ map (lmap Index)
  [ 1 /\ Day Day1.partOne Day1.partTwo
  , 2 /\ Day Day2.partOne Day2.partTwo
  , 3 /\ Day Day3.partOne Day3.partTwo
  , 4 /\ Day Day4.partOne Day4.partTwo
  , 5 /\ Day Day5.partOne Day5.partTwo
  , 6 /\ Day Day6.partOne Day6.partTwo
  , 7 /\ Day Day7.partOne Day7.partTwo
  , 8 /\ Day Day8.partOne Day8.partTwo
  , 9 /\ Day Day9.partOne Day9.partTwo
  , 10 /\ Day Day10.partOne Day10.partTwo
  , 11 /\ Day Day11.partOne Day11.partTwo
  , 12 /\ Day Day12.partOne Day12.partTwo
  , 13 /\ Day Day13.partOne Day13.partTwo
  , 14 /\ Day Day14.partOne Day14.partTwo
  , 15 /\ Day Day15.partOne Day15.partTwo
  , 16 /\ Day Day16.partOne Day16.partTwo
  , 17 /\ Day Day17.partOne Day17.partTwo
  , 18 /\ Day Day18.partOne Day18.partTwo
  , 19 /\ Day Day19.partOne Day19.partTwo
  , 20 /\ Day Day20.partOne Day20.partTwo
  , 21 /\ Day Day21.partOne Day21.partTwo
  , 22 /\ Day Day22.partOne Day22.partTwo
  ]
