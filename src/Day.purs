module Day (Day(..), Part, PartName(..), Year(..)) where

import Data.Either (Either)
import Data.Show (class Show)
import Effect.Exception (Error)

type Part = String -> Either Error String

data Day = Day Part Part

data Year = Year String (Array Day)

data PartName = PartOne | PartTwo

instance showPartName :: Show PartName where
  show PartOne = "PartOne"
  show PartTwo = "PartTwo"
