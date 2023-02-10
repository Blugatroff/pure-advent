module Year2022 (days) where

import Data.Function (($))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Day (Day(..))
import Year2022.Day1 as Day1
import Year2022.Day10 as Day10
import Year2022.Day11 as Day11
import Year2022.Day12 as Day12
import Year2022.Day13 as Day13
import Year2022.Day14 as Day14
import Year2022.Day15 as Day15
import Year2022.Day18 as Day18
import Year2022.Day2 as Day2
import Year2022.Day3 as Day3
import Year2022.Day4 as Day4
import Year2022.Day5 as Day5
import Year2022.Day6 as Day6
import Year2022.Day7 as Day7
import Year2022.Day8 as Day8
import Year2022.Day9 as Day9

days = Map.fromFoldable
  [ Tuple 1 $ Day Day1.partOne Day1.partTwo
  , Tuple 2 $ Day Day2.partOne Day2.partTwo
  , Tuple 3 $ Day Day3.partOne Day3.partTwo
  , Tuple 4 $ Day Day4.partOne Day4.partTwo
  , Tuple 5 $ Day Day5.partOne Day5.partTwo
  , Tuple 6 $ Day Day6.partOne Day6.partTwo
  , Tuple 7 $ Day Day7.partOne Day7.partTwo
  , Tuple 8 $ Day Day8.partOne Day8.partTwo
  , Tuple 9 $ Day Day9.partOne Day9.partTwo
  , Tuple 10 $ Day Day10.partOne Day10.partTwo
  , Tuple 11 $ Day Day11.partOne Day11.partTwo
  , Tuple 12 $ Day Day12.partOne Day12.partTwo
  , Tuple 13 $ Day Day13.partOne Day13.partTwo
  , Tuple 14 $ Day Day14.partOne Day14.partTwo
  , Tuple 15 $ Day Day15.partOne Day15.partTwo
  , Tuple 18 $ Day Day18.partOne Day18.partTwo
  ]
