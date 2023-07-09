module Year2022.Day23 (day) where

import MeLude

import Data.Array as Array
import Data.HashMap as HM
import Data.HashSet as HS
import Data.TraversableWithIndex (forWithIndex)
import Day (makeDay)
import Util (dedupCount, nonEmptyLines)

type Board = HS.HashSet (Int /\ Int)

parse :: String -> String |? Board
parse input = HS.fromFoldable <<< Array.catMaybes <<< Array.concat <$> do
  forWithIndex (nonEmptyLines input) \y line -> do
    forWithIndex (toCharArray line) \x -> case _ of
      '.' -> Right Nothing
      '#' -> Right $ Just (x /\ y)
      c -> Left $ "unknown tile: '" <> show c <> "'"

rotateArray :: forall a. Array a -> Array a
rotateArray a = Array.drop 1 a <> Array.take 1 a

rotateArrayN :: forall a. Int -> Array a -> Array a
rotateArrayN n a | n == Array.length a = a
rotateArrayN n a | n > Array.length a = rotateArrayN (n `mod` Array.length a) a
rotateArrayN n a = applyN rotateArray n a

countMatching :: forall f a. Foldable f => (a -> Boolean) -> f a -> Int
countMatching condition = foldl (\n a -> if condition a then n + 1 else n) 0

type Pos = Int /\ Int
type Modification = Pos /\ Pos

runRound :: Int -> Board -> Board
runRound roundNumber board =
  foldl applyRemoval board freeModifications
    # \board -> foldl applyInsertion board freeModifications
  where
  elves = HS.toUnfoldable board

  applyRemoval board = fst >>> flip HS.delete board
  applyInsertion board = snd >>> flip HS.insert board

  freeModifications :: Array Modification
  freeModifications = Array.filter f modifications
    where
    insertions = HM.fromFoldable (dedupCount (map snd modifications) :: Array _)
    f (_ /\ dst) = HM.lookup dst insertions == Just 1

  modifications :: Array Modification
  modifications = do
    let props = proposals board
    flip Array.mapMaybe elves \elve -> do
      props <- props elve
      Array.findMap identity $ rotateArrayN roundNumber $ props

  proposals :: Board -> Pos -> Maybe (Array (Maybe Modification))
  proposals board elve@(x /\ y) = do
    let northWest = (x - 1) /\ (y - 1)
    let north = x /\ (y - 1)
    let northEast = (x + 1) /\ (y - 1)
    let west = (x - 1) /\ y
    let east = (x + 1) /\ y
    let southWest = (x - 1) /\ (y + 1)
    let south = x /\ (y + 1)
    let southEast = (x + 1) /\ (y + 1)
    let lookupOrEmpty = flip HS.member board
    let nw = lookupOrEmpty northWest
    let no = lookupOrEmpty north
    let ne = lookupOrEmpty northEast
    let we = lookupOrEmpty west
    let ea = lookupOrEmpty east
    let sw = lookupOrEmpty southWest
    let so = lookupOrEmpty south
    let se = lookupOrEmpty southEast
    if not (nw || no || ne || we || ea || sw || so || se) then Nothing else Just unit
    Just
      [ case nw, no, ne, we, ea, sw, so, se of
          false, false, false, _, _, _, _, _ -> Just (elve /\ north)
          _, _, _, _, _, _, _, _ -> Nothing
      , case nw, no, ne, we, ea, sw, so, se of
          _, _, _, _, _, false, false, false -> Just (elve /\ south)
          _, _, _, _, _, _, _, _ -> Nothing
      , case nw, no, ne, we, ea, sw, so, se of
          false, _, _, false, _, false, _, _ -> Just (elve /\ west)
          _, _, _, _, _, _, _, _ -> Nothing
      , case nw, no, ne, we, ea, sw, so, se of
          _, _, false, _, false, _, _, false -> Just (elve /\ east)
          _, _, _, _, _, _, _, _ -> Nothing
      ]

countEmpty :: Board -> Int
countEmpty board = countMatching not do
  y <- Array.range minY maxY
  x <- Array.range minX maxX
  pure $ HS.member (x /\ y) board
  where
  positions = HS.toUnfoldable board :: Array (Int /\ Int)
  xs = map fst positions
  ys = map snd positions
  minX = fromMaybe 0 $ minimum xs
  minY = fromMaybe 0 $ minimum ys
  maxX = fromMaybe 0 $ maximum xs
  maxY = fromMaybe 0 $ maximum ys

solvePartOne :: Int -> Board -> Int
solvePartOne rounds board = countEmpty $ foldl (flip runRound) board (Array.range 0 (rounds - 1))

solvePartTwo = add 1 <<< f 0
  where
  f n board = if board == newBoard then n else f (n + 1) newBoard
    where
    newBoard = runRound n board

day = makeDay parse
  (Right <<< show <<< solvePartOne 10)
  (Right <<< show <<< solvePartTwo)
