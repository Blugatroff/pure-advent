module Year2022.Day7 (partOne, partTwo) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), drop, filter, fromFoldable, head, reverse, sort, (:))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (greaterThanOrEq, lessThanOrEq)
import Data.String (Pattern(..), trim)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (foldl, sum, traverse)
import Data.Tuple (Tuple(..), snd)
import Effect.Exception (Error, error)
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
      entriesString = M.toUnfoldable entries <#> (\(Tuple name child) -> "\n" <> indent (name <> ": " <> show child)) # String.joinWith "" # (_ `append` "\n")

parseLine :: List Char -> Either Error TerminalLine
parseLine line@('$':command) = case fromFoldable $ toCharArray $ trim $ fromCharArray $ Array.fromFoldable command of
  'l':'s':Nil -> Right LsLine
  'c':'d':args -> case trim $ fromCharArray $ Array.fromFoldable args of
    ".." -> Right $ CdLine TargetUp
    "/" -> Right $ CdLine TargetRoot
    folder -> Right $ CdLine $ TargetDown folder
  _ -> Left $ error $ "unknown command " <> fromCharArray (Array.fromFoldable line)
parseLine ('d':'i':'r':folder) = Right $ DirLine $ trim $ fromCharArray $ Array.fromFoldable folder
parseLine line = case splitStringOnce " " $ fromCharArray $ Array.fromFoldable line of
  Just (Tuple size name) -> parseInt size <#> FileLine (trim name)
  Nothing -> Left $ error $ "Failed to parse command " <> (fromCharArray $ Array.fromFoldable line)

parse :: String -> Either Error (Array TerminalLine)
parse input = String.split (Pattern "\n") input <#> trim # Array.filter (not <<< String.null) # traverse (toCharArray >>> fromFoldable >>> parseLine)

fileSystemSize :: FileSystem -> Int
fileSystemSize (File _ size) = size
fileSystemSize (Directory entries) = M.values entries <#> fileSystemSize # sum

insertEntry :: List String -> (Maybe FileSystem -> FileSystem) -> FileSystem -> FileSystem
insertEntry Nil new sys = new $ Just sys
insertEntry (_:_) _ sys@(File _ _) = sys
insertEntry (next:Nil) new (Directory entries) = Directory $ M.alter (Just <<< new) next entries
insertEntry (next:rest) new (Directory entries) = Directory $ M.update (Just <<< insertEntry rest new) next entries

assembleFileSystem :: forall l. Foldable l => l TerminalLine -> FileSystem
assembleFileSystem = snd <<< foldl fold (Tuple Nil (Directory M.empty))
  where
    fold :: Tuple (List String) FileSystem -> TerminalLine -> Tuple (List String) FileSystem
    fold state LsLine = state
    fold (Tuple cwd sys) (CdLine TargetUp) = Tuple (drop 1 cwd) sys
    fold (Tuple _ sys) (CdLine TargetRoot) = Tuple Nil sys
    fold (Tuple cwd sys) (CdLine (TargetDown dir)) = Tuple (dir:cwd) sys
    fold (Tuple cwd sys) (FileLine name size) = Tuple cwd (insertEntry (reverse $ name:cwd) (const $ File name size) sys)
    fold (Tuple cwd sys) (DirLine name) = Tuple cwd (insertEntry (reverse $ name:cwd) (fromMaybe $ Directory M.empty) sys)

fileSystemEntries :: FileSystem -> List FileSystem
fileSystemEntries file@(File _ _) = file:Nil
fileSystemEntries dir@(Directory children) = dir:(M.values children >>= fileSystemEntries)

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

partOne :: String -> Either Error String
partOne input = parse input <#> solvePartOne <#> show

partTwo :: String -> Either Error String
partTwo input = parse input <#> solvePartTwo <#> show
