module Year2022.Day10 (partOne, partTwo) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable, sum)
import Data.List (List, (:))
import Data.List as List
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import Util (lines, parseInt, splitStringOnce)

data Instruction = AddX Int | NoOp

derive instance instructionEq :: Eq Instruction

instance showInstruction :: Show Instruction where
  show NoOp = "NoOp"
  show (AddX n) = "(AddX " <> show n <> ")"

parseLine :: String -> Either Error Instruction
parseLine "noop" = Right NoOp
parseLine line = case splitStringOnce " " line of
  Nothing -> Left $ error $ "failed to parse line: " <> line
  Just (Tuple "addx" argument) -> parseInt argument <#> AddX
  Just (Tuple instruction _) -> Left $ error $ "unknown instruction: " <> instruction

parse :: String -> Either Error (Array Instruction)
parse input = lines input <#> String.trim # Array.filter (not <<< String.null) # traverse parseLine

data CpuState = Idle | Adding { cyclesLeft :: Int, value :: Int }

derive instance cpuStateEq :: Eq CpuState

data Cpu = Cpu
  { state :: CpuState
  , xRegister :: Int
  , cycleNumber :: Int
  , instructions :: List Instruction
  }

instance showCpu :: Show Cpu where
  show (Cpu cpu) = "(Cpu { cycle: " <> show cpu.cycleNumber <> ", x: " <> show cpu.xRegister <> " })"

executeCycle :: Cpu -> Maybe Cpu
executeCycle (Cpu cpu) = case cpu.state of
  Idle -> case cpu.instructions of
    List.Nil -> Nothing
    (instruction : remainingInstructions) -> case instruction of
      NoOp -> Just $ Cpu $ cpu { instructions = remainingInstructions, cycleNumber = cpu.cycleNumber + 1 }
      AddX value -> executeCycle $ Cpu $ cpu { instructions = remainingInstructions, cycleNumber = cpu.cycleNumber, state = Adding { cyclesLeft: 2, value } }
  Adding { cyclesLeft: 0, value } -> executeCycle $ Cpu $ cpu { state = Idle, xRegister = cpu.xRegister + value }
  Adding { cyclesLeft, value } -> Just $ Cpu $ cpu { state = Adding { cyclesLeft: cyclesLeft - 1, value: value }, cycleNumber = cpu.cycleNumber + 1 }

samplePoints = [ 20, 60, 100, 140, 180, 220 ]

runCpu :: forall a. (Cpu -> a -> a) -> a -> Cpu -> a
runCpu f state cpu = case executeCycle cpu of
  Nothing -> state
  Just newCpu -> runCpu f (f newCpu state) newCpu

solvePartOne :: forall f. Foldable f => f Instruction -> Int
solvePartOne instructions = sum $ runCpu sample List.Nil cpu
  where
  cpu = Cpu { state: Idle, xRegister: 1, cycleNumber: 0, instructions: List.fromFoldable instructions }
  sample (Cpu cpu) samples =
    if cpu.cycleNumber `Array.elem` samplePoints then (cpu.xRegister * cpu.cycleNumber) : samples
    else samples

type Crt = M.Map (Tuple Int Int) Boolean

crtWidth = 40
crtHeight = 6

drawCrt :: Crt -> String
drawCrt crt = Array.intercalate "\n" $
  Array.range 0 (crtHeight - 1) <#> \y ->
    fromCharArray $ Array.range 0 (crtWidth - 1) <#> \x ->
      case M.lookup (Tuple x y) crt of
        Nothing -> '.'
        Just true -> '#'
        Just false -> '.'

solvePartTwo :: forall f. Foldable f => f Instruction -> String
solvePartTwo instructions = drawCrt $ runCpu sample M.empty cpu
  where
  cpu = Cpu { state: Idle, xRegister: 1, cycleNumber: 0, instructions: List.fromFoldable instructions }
  sample (Cpu cpu) crt = M.insert (Tuple x y) value crt
    where
    x = (cpu.cycleNumber - 1) `mod` crtWidth
    y = (cpu.cycleNumber - 1) `div` crtWidth
    value = (x >= (cpu.xRegister - 1)) && (x <= (cpu.xRegister + 1))

partOne :: String -> Either Error String
partOne input = parse input <#> solvePartOne <#> show

partTwo :: String -> Either Error String
partTwo input = parse input <#> solvePartTwo
