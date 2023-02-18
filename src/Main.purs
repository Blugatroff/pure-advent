module Main (main) where

import Prelude

import Control.Monad.Error.Class (liftEither, try)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Day (Day(..), PartName(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, error, message)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv, exit)
import Util (parseInt)
import Year2022 as Year2022

arrayToList :: forall a. Array a -> List a
arrayToList array = List.fromFoldable array

main :: Effect Unit
main = do
  args :: List String <- arrayToList <$> argv
  arguments <- case parseArgs args of
    Right arguments -> pure arguments
    Left error -> do
      Console.error error
      Console.log ""
      printHelp
      exit 1
  result <- runExceptT $ start arguments
  case result of
    Left errorMessage -> do
      Console.error $ message errorMessage
      exit 1
    Right _ -> pure unit

printHelp :: Effect Unit
printHelp = do
  Console.log "Advent of Code in Purescript"
  Console.log "Usage:"
  Console.log "    aoc all"
  Console.log "    aoc <year> <day> [part] [OPTIONS]"
  Console.log ""
  Console.log "OPTIONS:"
  Console.log "    -i, --input <file>"
  Console.log "           Load the puzzle input from the specified file."

start :: Arguments -> ExceptT Error Effect Unit
start Help = liftEffect printHelp
start (RunDay year dayIndex partName file) = do
  part <- case Map.lookup (unIndex dayIndex) Year2022.days of
    Just (Day partOne partTwo) -> pure $ case partName of
      PartOne -> partOne
      PartTwo -> partTwo
    Nothing -> liftEither $ Left $ error $ "The day " <> show dayIndex <> " does not exist! (yet?)"

  input <- case file of
    Nothing -> ExceptT $ readInputFile $ buildPath year dayIndex
    Just inputFile -> ExceptT $ readInputFile inputFile

  result <- liftEither $ part input
  liftEffect $ Console.log result
start RunAll = do
  let days = Array.sortBy (compare `on` fst) $ Map.toUnfoldable Year2022.days
  for_ days $ \(Tuple i (Day partOne partTwo)) -> do
    liftEffect $ Console.log $ "Day " <> show i

    input <- ExceptT $ readInputFile $ buildPath 2022 i

    resultPartOne <- liftEither $ partOne input
    liftEffect $ Console.log $ resultPartOne

    resultPartTwo <- liftEither $ partTwo input
    liftEffect $ Console.log $ resultPartTwo

    liftEffect $ Console.log ""
    pure unit

readInputFile ∷ String → Effect (Either Error String)
readInputFile path = (try $ readTextFile UTF8 path)
  <#> lmap \e -> error $ "failed to read the input file: " <> message e

buildPath :: forall year day. Show year => Show day => year -> day -> String
buildPath year day = "." <> sep <> "inputs" <> sep <> show year <> sep <> show day <> ".txt"
  where
  sep = "/"

data Index :: forall k. k -> Type
data Index a = Index Int

unIndex :: forall a. Index a -> Int
unIndex (Index i) = i

instance showIndex :: Show (Index a) where
  show (Index index) = show index

data YearName = TheYear2022

instance showYearName :: Show YearName where
  show TheYear2022 = "2022"

data Arguments = Help | RunDay YearName (Index Day) PartName (Maybe String) | RunAll

setFile :: String -> Arguments -> Arguments
setFile _ Help = Help
setFile _ RunAll = RunAll
setFile file (RunDay year day part _) = RunDay year day part (Just file)

instance showArguments :: Show Arguments where
  show Help = "Help"
  show (RunDay year day part input) = "(RunDay " <> show year <> " " <> show day <> " " <> show part <> " " <> show input <> ")"
  show RunAll = "RunAll"

parseArgs :: List String -> Either String Arguments
parseArgs = List.drop 2 >>> findFileArg
  where
  findFileArg :: List String -> Either String Arguments
  findFileArg args =
    let
      { init, rest } = List.span (\s -> s /= "-i" && s /= "--input") args
    in
      case rest of
        Nil -> inner args
        (_ : Nil) -> Left "expected path to input file after '-p'"
        (_ : path : rest) -> inner (init <> rest) <#> setFile path

  inner ("help" : Nil) = Right Help
  inner ("all" : Nil) = Right RunAll
  inner (year : day : Nil) = inner (year : day : "one" : Nil)
  inner ("2022" : dayIndex : partIndex : Nil) = do
    day <- lmap message $ Index <$> parseInt dayIndex
    part <- parsePart partIndex
    Right $ RunDay TheYear2022 day part Nothing
  inner args = Left $ "Unexpected Arguments: " <> (show $ Array.fromFoldable args)

  parsePart :: String -> Either String PartName
  parsePart "one" = Right PartOne
  parsePart "1" = Right PartOne
  parsePart "two" = Right PartTwo
  parsePart "2" = Right PartTwo
  parsePart s = Left $ "\"" <> s <> "\" is not a valid part, use 'one', 'two', '1' or '2'"
