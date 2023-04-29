module InputLoading where

import MeLude

import Data.Array as Array
import Data.List as List
import Data.String as String
import Day (Day, Index, YearName)
import Effect.Aff (Aff, Canceler(..), makeAff, throwError, try)
import Effect.Console as Console
import Effect.Exception (Error, error, message)
import Effect.Ref as Ref
import Fetch (Method(..))
import Fetch as Fetch
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir', readTextFile, writeTextFile)
import Node.FS.Perms (mkPerm, mkPerms)
import Node.Process (exit, lookupEnv, stdin)
import Node.Stream (onDataString, onEnd, onError)

newtype Cookie = Cookie String

rwx = mkPerm true true true
mode = mkPerms rwx rwx rwx

fetchInput :: YearName -> Index Day -> Aff String
fetchInput year day = do
  (Cookie cookie) <- liftEffect $ getCookie
  response <- Fetch.fetch url
    { method: GET
    , headers: { "Cookie": "session=" <> cookie }
    }
  input <- case response.ok of
    true -> response.text
    false -> throwError $ error $ response.statusText
  mkdir' ("./inputs/" <> show year) { mode, recursive: true }
  writeTextFile UTF8 (buildPath year day) input
  pure input
  where
  url = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"

loadInput :: YearName -> Index Day -> Aff String
loadInput year day =
  readInputFile year day >>= case _ of
    Right input -> pure input
    Left _ -> fetchInput year day

getCookie :: Effect Cookie
getCookie = lookupEnv "COOKIE" >>= case _ of
  Just cookie -> pure $ Cookie cookie
  Nothing -> do
    Console.error "COOKIE environment variable missing"
    exit 1

readInputFile ∷ YearName -> Index Day → Aff (Either Error String)
readInputFile year day = (try $ readTextFile UTF8 $ buildPath year day)
  <#> lmap \e -> error $ "failed to read the input file: " <> message e

buildPath ∷ YearName → Index Day → String
buildPath year day = "." <> sep <> "inputs" <> sep <> show year <> sep <> show day <> ".txt"
  where
  sep = "/"

readStdinAll :: Aff String
readStdinAll = makeAff \resolve -> do
  chunks <- Ref.new List.Nil
  liftEffect do
    onError stdin $ \error -> do
       resolve $ Left error
    onDataString stdin UTF8 \chunk -> do
      void $ Ref.modify (chunk:_) chunks
    onEnd stdin do
      resolve =<< Right <<< String.joinWith "" <<< Array.reverse <<< Array.fromFoldable <$> Ref.read chunks
  pure $ Canceler \_ -> pure unit
