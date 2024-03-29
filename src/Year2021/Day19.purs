module Year2021.Day19 (day) where

import MeLude

import Control.Monad.State (State)
import Control.Monad.State as State
import Data.Array as Array
import Data.CodePoint.Unicode (isNumber)
import Data.Pos3 (Pos3(..), manhattan)
import Data.String as String
import Day (makeDayWithCommonPart)
import Util (dedup, nonEmptyLines, parseInt, traceRuntime, tuplePermutations)

type Scanner = { id :: Int, beacons :: Array (Pos3 Int) }

parseScanner :: String -> String |? Scanner
parseScanner str = do
  let lines = nonEmptyLines str
  header <- note ("failed to parse scanner: " <> str) $ Array.head lines

  let digits = Array.filter (codePointFromChar >>> isNumber) $ toCharArray header
  let error = "failed to parse scanner id of scanner: " <> str
  id <- lmap (const error) $ parseInt $ fromCharArray digits

  tail <- note ("failed to parse scanner: " <> str) $ Array.tail lines
  beacons <- traverse parsePos tail
  Right { id, beacons }

parsePos :: String -> String |? Pos3 Int
parsePos str = do
  splits <- traverse parseInt $ String.split (String.Pattern ",") $ String.trim str
  case splits of
    [ x, y, z ] -> Right $ Pos3 x y z
    _ -> Left $ "failed to parse pos: " <> str

parse :: String -> String |? Array Scanner
parse = traverse parseScanner <<< String.split (String.Pattern "\n\n")

type Orientation = Pos3 Int -> Pos3 Int

rotateAroundX (Pos3 x y z) = Pos3 x (-z) y
rotateAroundY (Pos3 x y z) = Pos3 (-z) y x
rotateAroundZ (Pos3 x y z) = Pos3 (-y) x z

allOrientations :: Array Orientation
allOrientations = Array.concat do
  [ Array.range 0 3 <#> \n -> applyN rotateAroundX 0 >>> applyN rotateAroundZ n
  , Array.range 0 3 <#> \n -> applyN rotateAroundX 1 >>> applyN rotateAroundY n
  , Array.range 0 3 <#> \n -> applyN rotateAroundX 2 >>> applyN rotateAroundZ n
  , Array.range 0 3 <#> \n -> applyN rotateAroundX 3 >>> applyN rotateAroundY n
  , Array.range 0 3 <#> \n -> applyN rotateAroundY 1 >>> applyN rotateAroundX n
  , Array.range 0 3 <#> \n -> applyN rotateAroundY 3 >>> applyN rotateAroundX n
  ]

findOverlapAtSameOrientation :: Array (Pos3 Int) -> Array (Pos3 Int) -> Maybe (Pos3 Int)
findOverlapAtSameOrientation beaconsA beaconsB =
  flip Array.findMap beaconsB \beaconB -> Array.findMap (findMatches beaconB) beaconsA
  where
  findMatches beaconB beaconA = do
    let offset = beaconA - beaconB
    let pred p = Array.elem p beaconsA
    let nMatches = countMatching (add offset >>> pred) beaconsB
    if nMatches == 12 then Just offset else Nothing

findOverlap :: Array (Pos3 Int) -> Array (Pos3 Int) -> Maybe (Array (Pos3 Int) /\ Pos3 Int)
findOverlap beaconsA beaconsB = do
  flip Array.findMap allOrientations \orientation -> do
    let reorientedBeaconsB = map orientation beaconsB
    offset <- findOverlapAtSameOrientation beaconsA reorientedBeaconsB
    Just $ reorientedBeaconsB /\ offset

countMatching :: forall a. (a -> Boolean) -> Array a -> Int
countMatching pred = foldl (\n a -> if pred a then n + 1 else n) 0

newtype Network = Network { scanner :: Scanner, overlaps :: Array Overlap }
type Overlap = { offset :: Pos3 Int, network :: Network }

scannersInNetwork :: Network -> Array (Pos3 Int /\ Scanner)
scannersInNetwork = go zero
  where
  go pos (Network { scanner, overlaps }) =
    Array.cons (pos /\ scanner) (overlaps >>= \overlap -> go (pos + overlap.offset) overlap.network)

buildNetwork :: Scanner -> State (Array Scanner) Network
buildNetwork start = do
  scanners <- State.modify $ Array.filter (\scanner -> scanner.id /= start.id)
  overlaps <- Array.catMaybes <$> for scanners \scanner -> do
    alreadyConnected <- State.gets $ all (_.id >>> notEq scanner.id)
    if alreadyConnected then do
      pure Nothing
    else do
      case findOverlap start.beacons scanner.beacons of
        Nothing -> pure $ Nothing
        Just (reorientedBeaconsB /\ offset) -> do
          let reorientedScanner = scanner { beacons = reorientedBeaconsB }
          nextNetwork <- buildNetwork reorientedScanner
          pure $ Just { offset, network: nextNetwork }
  pure $ Network { scanner: start, overlaps }

solve :: Array Scanner -> String |? Array (Pos3 Int /\ Scanner)
solve = traceRuntime "solve" \scanners -> do
  first <- note "need at least one scanner" $ Array.head scanners
  let network = traceRuntime "buildNetwork" (State.evalState (buildNetwork first)) scanners
  pure $ scannersInNetwork network

solvePartOne scanners = do
  let beacons = scanners >>= \(pos /\ scanner) -> map (add pos) scanner.beacons
  let uniqueBeacons = dedup beacons
  Array.length uniqueBeacons

solvePartTwo scanners = do
  let distances = map (uncurry manhattan) $ tuplePermutations $ map fst scanners
  note "need at least one sensor" $ maximum distances

day = makeDayWithCommonPart parse solve
  (Right <<< show <<< solvePartOne)
  (map show <<< solvePartTwo)

  
