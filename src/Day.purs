module Day
  ( Day(..)
  , Part
  , PartName(..)
  , Year(..)
  , YearName(..)
  , Index(..)
  ) where

import MeLude

type Part = String -> Either String String

data Day = Day Part Part

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

data YearName = TheYear2022

instance showYearName :: Show YearName where
  show TheYear2022 = "2022"
