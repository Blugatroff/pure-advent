module Year2022.Day9 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.List as List
import Data.List.NonEmpty as NE
import Data.NonEmpty ((:|))
import Data.Ord (abs)
import Data.String as String
import Util (dedup, lines, mapFst, mapSnd, mapWithPrevious, parseInt, sign, splitStringOnce)

data Instruction = U | D | L | R

instance showInstruction :: Show Instruction where
  show U = "U"
  show D = "D"
  show L = "L"
  show R = "R"

type Rope = NE.NonEmptyList (Int /\ Int)

parseDirection ∷ String → String |? Instruction
parseDirection "U" = Right U
parseDirection "D" = Right D
parseDirection "L" = Right L
parseDirection "R" = Right R
parseDirection direction = Left $ "failed to parse instruction: " <> direction

parseLine :: String -> String |? (Array Instruction)
parseLine s = case String.trim s # splitStringOnce " " of
  Just (dirString /\ countString) -> do
    direction <- parseDirection dirString
    count <- parseInt countString
    pure $ Array.replicate count direction
  Nothing -> Left $ "Failed to parse line: " <> s

parse :: String -> String |? (Array Instruction)
parse input = lines input <#> String.trim # Array.filter (not <<< String.null) # traverse parseLine <#> Array.concat

pull :: Int /\ Int -> Int /\ Int -> Int /\ Int
pull (headX /\ headY) (tailX /\ tailY) =
  let
    xDistance = abs (headX - tailX)
    yDistance = abs (headY - tailY)
    xDirection = sign (headX - tailX)
    yDirection = sign (headY - tailY)
  in
    case 0 of
      _ | xDistance <= 1 && yDistance <= 1 -> tailX /\ tailY
      _ | xDistance == 2 && yDistance == 0 -> (tailX + xDirection) /\ tailY
      _ | xDistance == 0 && yDistance == 2 -> tailX /\ (tailY + yDirection)
      _ | otherwise -> (tailX + xDirection) /\ (tailY + yDirection)

executeInstruction :: Rope -> Instruction -> Rope
executeInstruction rope instruction = pullRope $
  case instruction of
    U -> updateHead (mapSnd $ (_ `sub` 1)) rope
    D -> updateHead (mapSnd $ add 1) rope
    L -> updateHead (mapFst $ (_ `sub` 1)) rope
    R -> updateHead (mapFst $ add 1) rope

  where
  updateHead :: (Int /\ Int -> Int /\ Int) -> Rope -> Rope
  updateHead f (NE.NonEmptyList (head :| body)) = NE.NonEmptyList $ f head :| body

  pullRope :: Rope -> Rope
  pullRope (NE.NonEmptyList (head :| body)) = NE.NonEmptyList $ head :| mapWithPrevious pull head body

solve :: forall f. Foldable f => Int -> f Instruction -> Int
solve tailLength instructions = foldl fold (startRope /\ Nil) instructions # snd # dedup # Array.length
  where
  startRope = NE.NonEmptyList ((0 /\ 0) :| (List.fromFoldable $ Array.replicate tailLength (0 /\ 0)))

  fold :: Rope /\ (List (Int /\ Int)) -> Instruction -> Rope /\ (List (Int /\ Int))
  fold (rope /\ path) instruction = movedRope /\ (visited : path)
    where
    movedRope = executeInstruction rope instruction
    visited = NE.last movedRope

partOne :: String -> String |? String
partOne input = parse input <#> solve 1 <#> show

partTwo :: String -> String |? String
partTwo input = parse input <#> solve 9 <#> show
