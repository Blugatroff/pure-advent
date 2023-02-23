module Main (main) where

import MeLude

import Control.Monad.Error.Class (liftEither, try)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Data.Array as Array
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Day (Day(..), Index(..), PartName(..), YearName(..))
import Dotenv as DotEnv
import Effect.Aff (Aff, launchAff_)
import Effect.Console as Console
import Effect.Exception (message)
import InputLoading (loadInput)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (exit)
import Options.Applicative (Parser, ParserInfo, ReadM, argument, command, eitherReader, execParser, fullDesc, help, helper, info, int, long, metavar, option, progDesc, short, showDefault, str, subparser, value, (<**>))
import Util (parseInt)
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

start :: Arguments -> ExceptT String Aff Unit
start (RunDay year dayIndex partName file) = do
  part <- case Map.lookup dayIndex Year2022.days of
    Just (Day partOne partTwo) -> pure $ case partName of
      PartOne -> partOne
      PartTwo -> partTwo
    Nothing -> throwError $ "The day " <> show dayIndex <> " does not exist! (yet?)"

  input <- case file of
    Nothing -> ExceptT $ map (lmap message) $ try $ loadInput year dayIndex
    Just inputFile -> ExceptT $ map (lmap message) $ try $ readTextFile UTF8 inputFile

  result <- liftEither $ part input
  liftEffect $ Console.log result
start RunAll = do
  let days = Array.sortBy (compare `on` fst) $ Map.toUnfoldable Year2022.days
  for_ days $ \(Tuple i (Day partOne partTwo)) -> do
    liftEffect $ Console.log $ "Day " <> show i

    input <- ExceptT $ map (lmap message) $ try $ loadInput TheYear2022 i

    liftEffect $ Console.log $ either identity identity $ partOne input
    liftEffect $ Console.log $ either identity identity $ partTwo input

    liftEffect $ Console.log ""
    pure unit

data Arguments = RunDay YearName (Index Day) PartName (Maybe String) | RunAll

parser :: ParserInfo Arguments
parser = info (commandsParser <**> helper) $ Array.fold
  [ fullDesc
  , progDesc "Advent of Code Solutions in Purescript"
  ]
  where
  commandsParser :: Parser Arguments
  commandsParser = subparser $ Array.fold
    [ command "all" (info (pure RunAll <**> helper) $ progDesc "Run all solutions")
    , command "run" (info (runDayParser <**> helper) $ progDesc "Run one particular solution")
    ]

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
