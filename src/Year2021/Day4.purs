module Year2021.Day4 (day) where

import MeLude

import Data.Array as Array
import Data.List as List
import Data.String as String
import Data.String.Utils (padStart)
import Day (makeDay)
import Util (chunks, parseInt)

newtype Slot = Slot { marked :: Boolean, n :: Int }

derive instance newtypeSlot :: Newtype Slot _

instance showSlot :: Show Slot where
  show (Slot { marked, n }) = show n <> (if marked then "+" else "-")

newtype Board = Board (Array (Array Slot))

derive instance newtypeBoard :: Newtype Board _

transposeBoard :: Board -> Board
transposeBoard = Board <<< Array.transpose <<< unwrap

showPadded :: forall a. Show a => Int -> a -> String
showPadded l = padStart l <<< show

showFoldablePadded :: forall f a. Functor f => Foldable f => Show a => Int -> f a -> String
showFoldablePadded l = intercalate "" <<< map (showPadded l)

instance showBoard :: Show Board where
  show (Board slots) = "Board [\n" <> (intercalate "\n" $ map (("\t" <> _) <<< showFoldablePadded 2) slots) <> "\n]"

data Game = Game { draws :: List Int, boards :: List Board }

instance showGame :: Show Game where
  show (Game game) = "(Game " <> show game <> ")"

replaceUntilNoChange :: String.Pattern -> String.Replacement -> String -> String
replaceUntilNoChange pat with list = if new == list then new else replaceUntilNoChange pat with new
  where
  new = String.replace pat with list

parseBoard :: Array String -> String |? Board
parseBoard lines = Board <$> traverse parseLine lines
  where
  minimizeWhitespace = replaceUntilNoChange (String.Pattern "  ") (String.Replacement " ")
  parseLine = traverse parseSlot <<< String.split (String.Pattern " ") <<< minimizeWhitespace
  parseSlot = map (\n -> Slot { marked: false, n }) <<< parseInt

parse :: String -> Either String Game
parse s = do
  (drawsLine /\ boardLines) <- case lines of
    (a : b) -> pure $ a /\ b
    _ -> Left $ "input is incomplete:\n" <> s
  draws <- map List.fromFoldable $ traverse parseInt $ String.split (String.Pattern ",") drawsLine
  boards <- traverse parseBoard $ map Array.fromFoldable $ chunks 5 $ map String.trim boardLines
  pure $ Game { draws, boards }
  where
  lines = List.filter (not <<< String.null) $ List.fromFoldable $ String.split (String.Pattern "\n") s

playRound :: Int -> Board -> Board
playRound draw = Board <<< map (map mapSlot) <<< unwrap
  where
  mapSlot (Slot { marked, n }) = Slot { marked: marked || (n == draw), n }

hasWonHorizontal :: Board -> Boolean
hasWonHorizontal (Board slots) = any (all (_.marked <<< unwrap)) slots

hasWon :: Board -> Boolean
hasWon board = hasWonHorizontal board || hasWonHorizontal (transposeBoard board)

slotNumber :: Slot -> Int
slotNumber = _.n <<< unwrap

slotMarked :: Slot -> Boolean
slotMarked = _.marked <<< unwrap

computeScore :: Int -> Board -> Int
computeScore draw board = draw * sum (map slotNumber $ Array.filter (not <<< slotMarked) $ Array.concat $ unwrap board)

playLoser :: Game -> Maybe { loser :: Board, draws :: List Int }
playLoser (Game { draws: List.Nil }) = Nothing
playLoser (Game { draws: (draw : draws), boards }) =
  case notWon of
    List.Nil -> Nothing
    (loser : List.Nil) -> Just { loser, draws }
    remaining -> playLoser (Game { draws, boards: remaining })
  where
  notWon = List.filter (not <<< hasWon) $ map (playRound draw) boards

play :: Game -> Maybe { winner :: Board, draw :: Int }
play (Game { draws: List.Nil }) = Nothing
play (Game { draws: (draw : draws), boards }) =
  case winningBoard of
    Nothing -> play (Game { draws, boards: newBoards })
    Just winner -> Just { winner, draw }
  where
  newBoards = map (playRound draw) boards
  winningBoard = List.find hasWon newBoards

solvePartOne :: Game -> Maybe Int
solvePartOne game = do
  { winner, draw } <- play game
  pure $ computeScore draw winner

solvePartTwo :: Game -> Maybe Int
solvePartTwo game = do
  { loser, draws } <- playLoser game
  solvePartOne (Game { draws, boards: List.singleton loser })

reportFailure :: forall a. Show a => Maybe a -> String
reportFailure Nothing = "No solution found"
reportFailure (Just a) = show a

day = makeDay parse
  (Right <<< reportFailure <<< solvePartOne)
  (Right <<< reportFailure <<< solvePartTwo)


