module Main (main) where

import MeLude

import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Data.Array as Array
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Day (Day(..), Index(..), PartName(..), YearName(..))
import Dotenv as DotEnv
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Effect.Exception (message)
import Effect.Now as Now
import Data.DateTime.Instant as Instant
import InputLoading (loadInput)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (exit)
import Options.Applicative (Parser, ParserInfo, ReadM, argument, command, eitherReader, execParser, fullDesc, help, helper, info, int, long, metavar, option, progDesc, short, showDefault, str, subparser, value, (<**>))
import Util (parseInt)
import Year2021 as Year2021
import Year2022 as Year2022

main :: Effect Unit
main = launchAff_ do
  DotEnv.loadFile
  arguments <- liftEffect $ execParser parser
  result <- runExceptT $ start arguments
  liftEffect $ case result of
    Left error -> do
      Console.error error
      exit 1
    Right _ -> pure unit

yearDays TheYear2021 = Year2021.days
yearDays TheYear2022 = Year2022.days

start :: Arguments -> ExceptT String Aff Unit
start (RunDay year dayIndex partName file) = do
  let days = yearDays year
  part <- case Map.lookup dayIndex days of
    Just (Day partOne partTwo) -> pure $ case partName of
      PartOne -> partOne
      PartTwo -> partTwo
    Nothing -> throwError $ "The day " <> show dayIndex <> " does not exist! (yet?)"

  input <- ExceptT $ map (lmap message) $ try case file of
    Nothing -> loadInput year dayIndex
    Just inputFile -> readTextFile UTF8 inputFile
  liftEffect $ runPart part input
start (RunAll year) = do
  let days = Array.sortBy (compare `on` fst) $ Map.toUnfoldable $ yearDays year
  for_ days $ \(Tuple i (Day partOne partTwo)) -> do
    liftEffect $ Console.log $ "Day " <> show i
    input <- ExceptT $ map (lmap message) $ try $ loadInput year i
    liftEffect do
      runPart partOne input
      runPart partTwo input
      Console.log ""
      pure unit

runPart :: (String -> String |? String) -> String -> Effect Unit
runPart part input = do
  result /\ duration <- measureRuntime (pure <<< part) input
  case result of
    Right result -> Console.log $ result <> "\nTook " <> show duration <> "ms"
    Left error -> Console.log $ error <> "\nFailed in  " <> show duration <> "ms"

measureRuntime :: forall m a b. MonadEffect m => (a -> m b) -> a -> m (b /\ Number)
measureRuntime computation a = do
  start <- liftEffect $ unwrap <<< Instant.unInstant <$> Now.now
  result <- computation a
  end <- liftEffect $ unwrap <<< Instant.unInstant <$> Now.now
  let duration = end - start
  pure $ result /\ duration

data Arguments = RunDay YearName (Index Day) PartName (Maybe String) | RunAll YearName

parser :: ParserInfo Arguments
parser = info (commandsParser <**> helper) $ Array.fold
  [ fullDesc
  , progDesc "Advent of Code Solutions in Purescript"
  ]
  where
  commandsParser :: Parser Arguments
  commandsParser = subparser $ Array.fold
    [ command "all" (info (runYearParser <**> helper) $ progDesc "Run all solutions")
    , command "run" (info (runDayParser <**> helper) $ progDesc "Run one particular solution")
    ]

  runYearParser :: Parser Arguments
  runYearParser = ado
    year <- argument readYear $ metavar "year"
    in RunAll year

  runDayParser :: Parser Arguments
  runDayParser = ado
    year <- argument readYear $ metavar "year"
    day <- argument int $ metavar "day"
    part <- option readPart $ Array.fold
      [ value PartOne
      , showDefault
      , help "Which part of the problem to solve"
      , long "part"
      , short 'p'
      ]
    input <- option (Just <$> str) $ Array.fold
      [ value Nothing
      , long "input"
      , short 'i'
      ]
    in RunDay year (Index day) part input

  readYear :: ReadM YearName
  readYear = eitherReader $ \year -> case parseInt year of
    Right 2021 -> Right TheYear2021
    Right 2022 -> Right TheYear2022
    Right year -> Left $ "There are no Advent of Code Solutions for the Year " <> show year
    _ -> Left $ "Can't parse as Year: `" <> year <> "`"

  readPart :: ReadM PartName
  readPart = eitherReader $ case _ of
    "one" -> Right PartOne
    "1" -> Right PartOne
    "two" -> Right PartTwo
    "2" -> Right PartTwo
    part -> Left $ "Can't parse as Part: `" <> part <> "` use `one`, `two`, `1` or  `2`"
