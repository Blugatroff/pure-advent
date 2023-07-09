module Year2022 (days) where

import MeLude

import Data.Map as Map
import Day (Day, Index(..))

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
import Year2022.Day23 as Day23
import Year2022.Day24 as Day24
import Year2022.Day25 as Day25
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
  [ 1 /\ Day1.day
  , 2 /\ Day2.day
  , 3 /\ Day3.day
  , 4 /\ Day4.day
  , 5 /\ Day5.day
  , 6 /\ Day6.day
  , 7 /\ Day7.day
  , 8 /\ Day8.day
  , 9 /\ Day9.day
  , 10 /\ Day10.day
  , 11 /\ Day11.day
  , 12 /\ Day12.day
  , 13 /\ Day13.day
  , 14 /\ Day14.day
  , 15 /\ Day15.day
  , 16 /\ Day16.day
  , 17 /\ Day17.day
  , 18 /\ Day18.day
  , 19 /\ Day19.day
  , 20 /\ Day20.day
  , 21 /\ Day21.day
  , 22 /\ Day22.day
  , 23 /\ Day23.day
  , 24 /\ Day24.day
  , 25 /\ Day25.day
  ]
