module Year2022.Day7 (day) where

import MeLude

import Data.Array as Array
import Data.List (drop, filter, fromFoldable, head, reverse, sort)
import Data.Map as M
import Data.String (Pattern(..), trim)
import Data.String as String
import Day (makeDay)
import Util (parseInt, splitStringOnce)

data CdTarget = TargetUp | TargetRoot | TargetDown String

instance showCdTarget :: Show CdTarget where
  show TargetUp = "TargetUp"
  show TargetRoot = "TargetRoot"
  show (TargetDown target) = "(TargetDown " <> target <> ")"

data TerminalLine = LsLine | CdLine CdTarget | FileLine String Int | DirLine String

instance showTerminalLine :: Show TerminalLine where
  show LsLine = "LsLine"
  show (CdLine target) = "(CdLine " <> show target <> ")"
  show (FileLine name size) = "(FileLine " <> name <> " " <> show size <> ")"
  show (DirLine dir) = "(DirLine " <> dir <> ")"

data FileSystem = Directory (M.Map String FileSystem) | File String Int

indent :: String -> String
indent = String.split (Pattern "\n") >>> map ("\t" `append` _) >>> String.joinWith "\n"

instance showFileSystem :: Show FileSystem where
  show (File name size) = "(File " <> name <> " " <> show size <> ")"
  show (Directory entries) = "(Directory " <> entriesString <> ")"
    where
    entriesString = M.toUnfoldable entries <#> (\(name /\ child) -> "\n" <> indent (name <> ": " <> show child)) # String.joinWith "" # (_ `append` "\n")

parseLine :: List Char -> String |? TerminalLine
parseLine line@('$' : command) = case fromFoldable $ toCharArray $ trim $ fromCharArray $ Array.fromFoldable command of
  'l' : 's' : Nil -> Right LsLine
  'c' : 'd' : args -> case trim $ fromCharArray $ Array.fromFoldable args of
    ".." -> Right $ CdLine TargetUp
    "/" -> Right $ CdLine TargetRoot
    folder -> Right $ CdLine $ TargetDown folder
  _ -> Left $ "unknown command " <> fromCharArray (Array.fromFoldable line)
parseLine ('d' : 'i' : 'r' : folder) = Right $ DirLine $ trim $ fromCharArray $ Array.fromFoldable folder
parseLine line = case splitStringOnce " " $ fromCharArray $ Array.fromFoldable line of
  Just (size /\ name) -> parseInt size <#> FileLine (trim name)
  Nothing -> Left $ "Failed to parse command " <> (fromCharArray $ Array.fromFoldable line)

parse :: String -> String |? (Array TerminalLine)
parse input = String.split (Pattern "\n") input <#> trim # Array.filter (not <<< String.null) # traverse (toCharArray >>> fromFoldable >>> parseLine)

fileSystemSize :: FileSystem -> Int
fileSystemSize (File _ size) = size
fileSystemSize (Directory entries) = M.values entries <#> fileSystemSize # sum

insertEntry :: List String -> (Maybe FileSystem -> FileSystem) -> FileSystem -> FileSystem
insertEntry Nil new sys = new $ Just sys
insertEntry (_ : _) _ sys@(File _ _) = sys
insertEntry (next : Nil) new (Directory entries) = Directory $ M.alter (Just <<< new) next entries
insertEntry (next : rest) new (Directory entries) = Directory $ M.update (Just <<< insertEntry rest new) next entries

assembleFileSystem :: forall l. Foldable l => l TerminalLine -> FileSystem
assembleFileSystem = snd <<< foldl fold (Nil /\ (Directory M.empty))
  where
  fold :: List String /\ FileSystem -> TerminalLine -> List String /\ FileSystem
  fold state LsLine = state
  fold (cwd /\ sys) (CdLine TargetUp) = drop 1 cwd /\ sys
  fold (_ /\ sys) (CdLine TargetRoot) = Nil /\ sys
  fold (cwd /\ sys) (CdLine (TargetDown dir)) = (dir : cwd) /\ sys
  fold (cwd /\ sys) (FileLine name size) = cwd /\ (insertEntry (reverse $ name : cwd) (const $ File name size) sys)
  fold (cwd /\ sys) (DirLine name) = cwd /\ (insertEntry (reverse $ name : cwd) (fromMaybe $ Directory M.empty) sys)

fileSystemEntries :: FileSystem -> List FileSystem
fileSystemEntries file@(File _ _) = file : Nil
fileSystemEntries dir@(Directory children) = dir : (M.values children >>= fileSystemEntries)

isDirectory :: FileSystem -> Boolean
isDirectory (File _ _) = false
isDirectory (Directory _) = true

solvePartOne :: forall l. Foldable l => l TerminalLine -> Int
solvePartOne = assembleFileSystem
  >>> fileSystemEntries
  >>> filter isDirectory
  >>> map fileSystemSize
  >>> filter (_ `lessThanOrEq` 100000)
  >>> sum

diskSize :: Int
diskSize = 70000000

updateSize :: Int
updateSize = 30000000

solvePartTwo :: forall l. Foldable l => l TerminalLine -> Int
solvePartTwo lines =
  fileSystemEntries system
    # filter isDirectory
    <#> fileSystemSize
    # filter (_ `greaterThanOrEq` needed)
    # sort
    # head
    # fromMaybe 0
  where
  system = assembleFileSystem lines
  needed = updateSize - (diskSize - fileSystemSize system)

day = makeDay parse
  (Right <<< show <<< solvePartOne)
  (Right <<< show <<< solvePartTwo)

