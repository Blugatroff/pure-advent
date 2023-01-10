module Year2022.Day9 (partOne, partTwo) where

import Prelude

import Data.Array (length, replicate)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Ord (abs)
import Data.String as String
import Data.Traversable (foldl, traverse)
import Data.Tuple (Tuple(..), snd)
import Effect.Exception (Error, error)
import Util (dedup, lines, mapFst, mapSnd, mapWithPrevious, parseInt, sign, splitStringOnce)

data Instruction = U | D | L | R

instance showInstruction :: Show Instruction where
  show U = "U"
  show D = "D"
  show L = "L"
  show R = "R"

type Rope = NE.NonEmptyList (Tuple Int Int)

parseDirection ∷ String → Either Error Instruction
parseDirection "U" = Right U
parseDirection "D" = Right D
parseDirection "L" = Right L
parseDirection "R" = Right R
parseDirection direction = Left $ error $ "failed to parse instruction: " <> direction

parseLine :: String -> Either Error (Array Instruction)
parseLine s = case String.trim s # splitStringOnce " " of
  Just (Tuple dirString countString) -> do
    direction <- parseDirection dirString
    count <- parseInt countString
    pure $ replicate count direction
  Nothing -> Left $ error $ "Failed to parse line: " <> s

parse :: String -> Either Error (Array Instruction)
parse input = lines input <#> String.trim # Array.filter (not <<< String.null) # traverse parseLine <#> Array.concat

pull :: Tuple Int Int -> Tuple Int Int -> Tuple Int Int
pull (Tuple headX headY) (Tuple tailX tailY) =
  let
    xDistance = abs (headX - tailX)
    yDistance = abs (headY - tailY)
    xDirection = sign (headX - tailX)
    yDirection = sign (headY - tailY)
  in
    case 0 of
      _ | xDistance <= 1 && yDistance <= 1 -> Tuple tailX tailY
      _ | xDistance == 2 && yDistance == 0 -> Tuple (tailX + xDirection) tailY
      _ | xDistance == 0 && yDistance == 2 -> Tuple tailX (tailY + yDirection)
      _ | otherwise -> Tuple (tailX + xDirection) (tailY + yDirection)

executeInstruction :: Rope -> Instruction -> Rope
executeInstruction rope instruction = pullRope $
  case instruction of
    U -> updateHead (mapSnd $ (_ `sub` 1)) rope
    D -> updateHead (mapSnd $ add 1) rope
    L -> updateHead (mapFst $ (_ `sub` 1)) rope
    R -> updateHead (mapFst $ add 1) rope

  where
  updateHead :: (Tuple Int Int -> Tuple Int Int) -> Rope -> Rope
  updateHead f (NE.NonEmptyList (head :| body)) = NE.NonEmptyList $ f head :| body

  pullRope :: Rope -> Rope
  pullRope (NE.NonEmptyList (head :| body)) = NE.NonEmptyList $ head :| mapWithPrevious pull head body

solve :: forall f. Foldable f => Int -> f Instruction -> Int
solve tailLength instructions = foldl fold (Tuple startRope Nil) instructions # snd # dedup # length
  where
  startRope = NE.NonEmptyList ((Tuple 0 0) :| (List.fromFoldable $ Array.replicate tailLength (Tuple 0 0)))

  fold :: Tuple Rope (List (Tuple Int Int)) -> Instruction -> Tuple Rope (List (Tuple Int Int))
  fold (Tuple rope path) instruction = Tuple movedRope (visited : path)
    where
    movedRope = executeInstruction rope instruction
    visited = NE.last movedRope

partOne :: String -> Either Error String
partOne input = parse input <#> solve 1 <#> show

partTwo :: String -> Either Error String
partTwo input = parse input <#> solve 9 <#> show
