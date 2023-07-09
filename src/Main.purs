module Main (main) where

import MeLude

import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Data.Array as Array
import Data.DateTime.Instant as Instant
import Data.Map as Map
import Data.Posix.Signal (Signal(..))
import Data.Tuple (Tuple(..))
import Day (Day(..), Index(..), PartName(..), YearName(..))
import Dotenv as DotEnv
import Effect.Aff (Aff, Canceler(..), forkAff, joinFiber, launchAff_, makeAff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Effect.Exception (message)
import Effect.Now as Now
import InputLoading (loadInput, readStdinAll)
import Node.Buffer as Buffer
import Node.ChildProcess (ExecOptions, ExecResult, defaultExecOptions, execFile, kill)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (argv, exit)
import Options.Applicative (Parser, ParserInfo, ReadM, argument, command, eitherReader, execParser, fullDesc, help, helper, info, int, long, metavar, option, progDesc, short, str, subparser, value, (<**>))
import Util (parseInt)
import Year2021 as Year2021
import Year2022 as Year2022

execFileAff :: String -> Array String -> ExecOptions -> Aff ExecResult
execFileAff executable args options = do
  makeAff \resolve -> do
    child <- liftEffect $ execFile executable args options \result -> do
      resolve $ Right result
    pure $ Canceler \_ -> liftEffect $ kill SIGINT child

spawnSelf :: Array String -> Aff (String /\ String)
spawnSelf args = do
  argv <- liftEffect $ argv
  result <- case Array.take 2 argv of
    [ node, self ] -> do
      execFileAff node (Array.cons self args) defaultExecOptions
    _ -> liftEffect $ do
      Console.error "Failed to spawn self"
      exit 1
  case result.error of
    Nothing -> liftEffect $ do
      stdout <- Buffer.toString UTF8 result.stdout
      stderr <- Buffer.toString UTF8 result.stderr
      pure $ stdout /\ stderr
    Just error -> throwError error

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
    Just (Day { partOne, partTwo, partOneAndTwo }) -> pure $ case partName of
      Nothing -> partOneAndTwo >>> map (\(one /\ two) -> one <> "\n" <> two)
      Just PartOne -> partOne
      Just PartTwo -> partTwo
    Nothing -> throwError $ "The day " <> show dayIndex <> " does not exist! (yet?)"

  input <- ExceptT $ map (lmap message) $ try case file of
    Nothing -> loadInput year dayIndex
    Just (File inputPath) -> readTextFile UTF8 inputPath
    Just Stdin -> readStdinAll
  liftEffect $ runPart part input
start (RunAll year) = ExceptT $ map Right $ runAllInParallel year

runAllInParallel :: YearName -> Aff Unit
runAllInParallel year = do
  let days = Array.sortBy (compare `on` fst) $ Map.toUnfoldable $ yearDays year
  fibers <- for days \(Tuple i _) -> do
    fiber <- forkAff $ try $ spawnSelf [ "run", show year, show i ]
    pure $ i /\ fiber
  for_ fibers \(i /\ fiber) -> do
    result <- joinFiber fiber
    liftEffect $ Console.log $ "Day " <> show i
    liftEffect $ case result of
      Left error -> Console.error $ message error
      Right (stdout /\ _) -> Console.log stdout

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

data InputSource = File String | Stdin

data Arguments = RunDay YearName (Index Day) (Maybe PartName) (Maybe InputSource) | RunAll YearName

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
    part <- option (Just <$> readPart) $ Array.fold
      [ value Nothing
      , help "Which part of the problem to solve"
      , long "part"
      , short 'p'
      ]
    input <- option (Just <$> readInputSource) $ Array.fold
      [ value Nothing
      , long "input"
      , short 'i'
      ]
    in RunDay year (Index day) part input

  readInputSource :: ReadM InputSource
  readInputSource = ado
    path <- str
    in
      case path of
        "-" -> Stdin
        path -> File path

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
