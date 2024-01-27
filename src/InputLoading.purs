module InputLoading where

import MeLude

import Data.Array as Array
import Data.List as List
import Data.String as String
import Day (Day, Index(..), YearName, noPartTwoMessage)
import Effect.Aff (Aff, Canceler(..), makeAff, throwError, try)
import Effect.Class.Console as Console
import Effect.Exception (Error, error, message)
import Effect.Ref as Ref
import Fetch (Method(..))
import Fetch as Fetch
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir', readTextFile, writeTextFile)
import Node.FS.Perms (mkPerm, mkPerms)
import Node.Process (exit, lookupEnv, stdin)
import Node.Stream (onDataString, onEnd, onError)
import Util (errorCode, splitStringOnce)

newtype Cookie = Cookie String

rwx = mkPerm true true true
mode = mkPerms rwx rwx rwx

fetchGetWithCookie :: String -> Aff String
fetchGetWithCookie url = do
  (Cookie cookie) <- liftEffect getCookie
  response <- Fetch.fetch url
    { method: GET
    , headers: { "Cookie": "session=" <> cookie }
    }
  case response.ok of
    true -> response.text
    false -> throwError $ error $ response.statusText

fetchInput :: YearName -> Index Day -> Aff String
fetchInput year day = do
  let url = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"
  input <- fetchGetWithCookie url
  mkdir' ("./inputs/" <> show year) { mode, recursive: true }
  writeTextFile UTF8 (buildPath year day) input
  pure input

fetchExpectedResultUncached :: YearName -> Index Day -> Aff (Maybe (String /\ String))
fetchExpectedResultUncached year day = do
  let url = "https://adventofcode.com/" <> show year <> "/day/" <> show day
  body <- fetchGetWithCookie url
  when (not $ String.contains (String.Pattern "[Log Out]") body) do
    throwError $ error "Not logged in to adventofcode.com update the COOKIE variable!"
  body
    # String.split (String.Pattern "\n")
    # Array.filter (String.contains (String.Pattern "Your puzzle answer was"))
    # Array.mapMaybe
        ( String.split (String.Pattern "<code>") >>> flip Array.index 1
            >=> (String.split (String.Pattern "</code>") >>> flip Array.index 0)
        )
    # pure <<< case _ of
        [ one, two ] -> Just (one /\ two)
        [ one ] | day == Index 25 -> Just (one /\ noPartTwoMessage)
        _ -> Nothing

fetchExpectedResult :: YearName -> Index Day -> Aff (Maybe (String /\ String))
fetchExpectedResult year day = do
  let path = buildPath year day <> ".expected"
  try (readTextFile UTF8 path) >>= case _ of
    Left err | Just "ENOENT" == errorCode err ->
      fetchExpectedResultUncached year day >>= case _ of
        Nothing -> pure Nothing
        Just (one /\ two) -> do
          writeTextFile UTF8 path (one <> "\n" <> two)
          pure $ Just (one /\ two)
    Left err -> throwError err
    Right a -> pure $ splitStringOnce "\n" a

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
      void $ Ref.modify (chunk : _) chunks
    onEnd stdin do
      resolve =<< Right <<< String.joinWith "" <<< Array.reverse <<< Array.fromFoldable <$> Ref.read chunks
  pure $ Canceler \_ -> pure unit
