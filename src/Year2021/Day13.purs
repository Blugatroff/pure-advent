module Year2021.Day13 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.Either (blush)
import Data.String as String
import Data.String.Utils (startsWith)
import Util (dedup, parseInt)

data Fold = FoldAlongX Int | FoldAlongY Int

type Input = 
  { dots :: Array (Int /\ Int)
  , folds :: Array Fold
  }

type PaperSize = (Int /\ Int)

type Dots = Array (Int /\ Int)

parseFold :: String -> String |? Fold
parseFold line = value <#> axis
  where
    axis = if String.contains (String.Pattern "x") line then FoldAlongX else FoldAlongY
    value = do 
      let splits = String.split (String.Pattern "=") line
      v <- note ("failed to parse line: " <> line) $ Array.index splits 1
      parseInt v

parseDot :: String -> String |? (Int /\ Int)
parseDot line = case String.split (String.Pattern ",") line <#> parseInt of
  [x, y] -> do
    px <- x
    py <- y
    pure (px /\ py)
  _ -> Left $ "failed to parse line: " <> line

parseLine :: String -> String |? (Fold |? (Int /\ Int))
parseLine line | "fold" `startsWith` line = parseFold line <#> Left
parseLine line = parseDot line <#> Right

parse :: String -> String |? Input
parse input = do
  lines <- String.split (String.Pattern "\n") input # Array.filter (not <<< String.null) # traverse parseLine
  let dots = lines # Array.mapMaybe hush
  let folds = lines # Array.mapMaybe blush
  pure $ { dots,  folds }

foldAlongX :: Int -> Dots -> Dots
foldAlongX pos = map move
  where
    move :: (Int /\ Int) -> (Int /\ Int)
    move (x /\ y) | x <= pos = (x /\ y)
    move (x /\ y) = (pos - (x - pos)) /\ y

foldAlongY :: Int -> Dots -> Dots
foldAlongY pos = map move
  where
    move :: (Int /\ Int) -> (Int /\ Int)
    move (x /\ y) | y <= pos = (x /\ y)
    move (x /\ y) = x /\ (pos - (y - pos))

foldPaper :: Dots -> Fold -> Dots
foldPaper dots = case _ of
  FoldAlongX x -> foldAlongX x dots
  FoldAlongY y -> foldAlongY y dots

solvePartOne :: Input -> Int
solvePartOne input = case Array.head input.folds of
  Nothing -> Array.length $ dedup $ input.dots 
  Just f -> Array.length $ dedup $ foldPaper input.dots f

solvePartTwo :: Input -> String
solvePartTwo input = foldl foldPaper input.dots input.folds # dedup # showPaper

showPaper :: Dots -> String
showPaper dots =
  fold $ map (_ <> "\n") $
    Array.range 0 height
      <#> \y ->
        Array.range 0 width
          # map (\x -> if (x /\ y) `Array.elem` dots then '#' else '.')
          # fromCharArray
  where
    (width /\ height) = measurePaper dots

measurePaper :: Dots -> PaperSize
measurePaper dots = (fromMaybe zero $ maximum $ map fst dots) /\ (fromMaybe zero $ maximum $ map snd dots)

partOne = parse >>> map (solvePartOne >>> show)
partTwo = parse >>> map solvePartTwo

