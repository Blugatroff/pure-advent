module Day
  ( Day(Day)
  , Part
  , BothParts
  , PartName(..)
  , Year(..)
  , YearName(..)
  , Index(..)
  , makeDay
  , makeDayWithCommonPart
  ) where

import MeLude

type Part = String -> String |? String
type BothParts = String -> String |? (String /\ String)

newtype Day = Day
  { partOne :: Part
  , partTwo :: Part
  , partOneAndTwo :: BothParts
  }

makeDay 
  :: forall input
   . (String -> String |? input) 
  -> (input -> String |? String) 
  -> (input -> String |? String) 
  -> Day
makeDay parse solvePartOne solvePartTwo = Day { partOne, partTwo, partOneAndTwo }
  where
  partOne = parse >=> solvePartOne
  partTwo = parse >=> solvePartTwo
  partOneAndTwo = parse >=> \input -> do
    a <- solvePartOne input
    b <- solvePartTwo input
    Right $ a /\ b

makeDayWithCommonPart
  :: forall input common
   . (String -> String |? input)
  -> (input -> String |? common)
  -> (common -> String |? String)
  -> (common -> String |? String)
  -> Day
makeDayWithCommonPart parse common solvePartOne solvePartTwo = Day { partOne, partTwo, partOneAndTwo }
  where
  partOne = parse >=> common >=> solvePartOne
  partTwo = parse >=> \input -> do
    common <- common input
    solvePartTwo common

  partOneAndTwo = parse >=> \input -> do
    common <- common input
    solution1 <- solvePartOne common
    solution2 <- solvePartTwo common
    Right $ solution1 /\ solution2

data Year = Year String (Array Day)

data PartName = PartOne | PartTwo

instance showPartName :: Show PartName where
  show PartOne = "PartOne"
  show PartTwo = "PartTwo"

newtype Index :: forall k. k -> Type
newtype Index a = Index Int

derive instance newtypeIndex :: Newtype (Index a) _

instance eqInddex :: Eq (Index a) where
  eq = eq `on` unwrap

instance ordIndex :: Ord (Index a) where
  compare = compare `on` unwrap

instance showIndex :: Show (Index a) where
  show (Index index) = show index

data YearName = TheYear2021 | TheYear2022

instance showYearName :: Show YearName where
  show TheYear2022 = "2022"
  show TheYear2021 = "2021"
