module Year2021.Day22 (day) where

import MeLude

import Data.Array as Array
import Data.Cuboid (Cuboid(..))
import Data.Cuboid as Cuboid
import Data.Range (Range, (..))
import Data.Range as Range
import Data.String as String
import Day (makeDay)
import JS.BigInt (BigInt, fromInt)
import Util (nonEmptyLines, parseInt, splitStringOnce)

data Change = On | Off

derive instance eqChange :: Eq Change

data Step = Step Change (Cuboid BigInt)

parse :: String -> String |? Array Step
parse = nonEmptyLines >>> traverse parseLine

parseLine :: String -> String |? Step
parseLine line = note ("failed to parse line: \"" <> line <> "\"") do
  change /\ range <- splitStringOnce " " line
  change <- case change of
    "on" -> Just On
    "off" -> Just Off
    _ -> Nothing
  cuboid <- parseCuboid range
  Just $ Step change cuboid

parseCuboid :: String -> Maybe (Cuboid BigInt)
parseCuboid s = traverse parseRange (String.split (String.Pattern ",") s) >>= case _ of
  [ x, y, z ] -> Just $ Cuboid x y z
  _ -> Nothing

parseRange :: String -> Maybe (Range BigInt)
parseRange s = do
  s /\ e <- splitStringOnce ".." s
  s <- s # toCharArray # Array.filter (flip Array.elem (toCharArray "-0123456789")) # fromCharArray # parseInt # hush
  e <- hush $ parseInt e
  Just $ Range.new (fromInt s) (fromInt e)

solvePartTwo :: Array Step -> BigInt
solvePartTwo steps = sum $ map (\(Step ch cu) -> Cuboid.volume cu * direction ch) $ foldl go [] steps
  where
  flip On = Off
  flip Off = On

  direction On = one
  direction Off = -one

  go :: Array Step -> Step -> Array Step
  go cuboids step = cuboids <> newCuboids
    where
    Step change cuboid = step
    intersections = cuboids # Array.mapMaybe \(Step ch cu) -> Step (flip ch) <$> Cuboid.intersection cuboid cu
    newCuboids = case change of
      On -> [ Step change cuboid ] <> intersections
      Off -> intersections

solvePartOne = solvePartTwo <<< Array.mapMaybe (\(Step ch cu) -> Step ch <$> Cuboid.intersection considered cu)
  where
  consideredRange = -(fromInt 50) .. (fromInt 50)
  considered = Cuboid consideredRange consideredRange consideredRange

day = makeDay parse (Right <<< show <<< solvePartOne) (Right <<< show <<< solvePartTwo)
