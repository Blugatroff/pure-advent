module Year2021.Day6 (day) where

import MeLude

import Data.List as List
import Data.List.Lazy as LazyList
import Data.Map as M
import Data.String as String
import Day (makeDay)
import JS.BigInt (BigInt, fromInt)
import Util (dedupCount, mapSnd, parseInt)

parse :: String -> String |? List Int
parse = traverse parseInt <<< List.fromFoldable <<< map String.trim <<< String.split (String.Pattern ",")

iterateMap :: forall v. v -> Int -> Map Int v -> List (Int /\ v)
iterateMap _ 0 _ = List.Nil
iterateMap def n map = (n /\ fromMaybe def (M.lookup n map)) : iterateMap def (n - 1) map

age :: Map Int BigInt -> Map Int BigInt
age = M.fromFoldable <<< map f <<< iterateMap zero 8
  where
    f :: (Int /\ BigInt) -> Int /\ BigInt
    f (0 /\ v) = 0 /\ v
    f (k /\ v) = (k - 1) /\ v

step :: Map Int BigInt -> Map Int BigInt
step fish = M.insertWith add 6 f0 $ M.insertWith add 8 f0 $ age $ M.insert 0 zero fish
  where
    f0 :: BigInt
    f0 = fromMaybe zero $ M.lookup 0 fish

solvePartTwo :: Int -> List Int -> BigInt
solvePartTwo days fish = fromMaybe zero 
  $ map (foldl add zero) 
  $ flip LazyList.index days
  $ LazyList.iterate step 
  $ M.fromFoldable 
  $ (map (mapSnd fromInt) (dedupCount fish) :: Array (Int /\ BigInt))

day = makeDay parse
  (Right <<< show <<< solvePartTwo 80)
  (Right <<< show <<< solvePartTwo 256)


