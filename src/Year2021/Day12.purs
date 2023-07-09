module Year2021.Day12 (day) where

import MeLude hiding ((:))

import Data.Array ((:))
import Data.Array as Array
import Data.CodePoint.Unicode (isUpper)
import Data.Foldable (foldr)
import Data.List as List
import Data.Map as M
import Data.Set as S
import Data.String (toCodePointArray)
import Data.String as String
import Day (makeDayWithCommonPart)
import Util (nonEmptyLines)

data Node = Small String | Big String

derive instance eqNode :: Eq Node

derive instance ordNode :: Ord Node

instance showNode :: Show Node where
  show = show <<< nodeName

type Link = Node /\ Node

type Cave = M.Map String (Array Node)

type Visited = List Node

nodeName :: Node -> String
nodeName (Small name) = name
nodeName (Big name) = name

node :: String -> Node
node str = case any isUpper $ toCodePointArray str of
  true -> Big str
  false -> Small str

parseLine :: String -> String |? Link
parseLine input = case map node $ String.split (String.Pattern "-") input of
  [ left, right ] -> Right (left /\ right)
  _ -> Left $ "failed to parse line: " <> input

parse = nonEmptyLines >>> traverse parseLine

addLinkToCave :: Link -> Cave -> Cave
addLinkToCave (left /\ right) = addRightToLeft <<< addLeftToRight
  where
  leftName = nodeName left
  rightName = nodeName right

  addRightToLeft cave = case M.lookup leftName cave of
    Just links -> M.insert leftName (right : links) cave
    Nothing -> M.insert leftName [ right ] cave
  addLeftToRight cave = case M.lookup rightName cave of
    Just links -> M.insert rightName (left : links) cave
    Nothing -> M.insert rightName [ left ] cave

buildCave = foldr addLinkToCave M.empty

countPaths joker = inner joker S.empty List.Nil (node "start") (node "end")
  where
  inner :: Boolean -> Set Node -> Visited -> Node -> Node -> Cave -> Int
  inner _ _ _ from to _ | from == to = 1
  inner hasUsedJoker visitedSet visited from to cave = do
    case M.lookup (nodeName from) cave of
      Nothing -> 0
      Just from -> sum $ map launchNext $ filterNeighbours $ from
    where
    launchNext :: Node /\ Boolean -> Int
    launchNext (neighbour /\ usedJoker) = inner usedJoker (S.insert from visitedSet) (List.Cons from visited) neighbour to cave

    filterNeighbours = Array.mapMaybe canBeVisitedAgain

    canBeVisitedAgain :: Node -> Maybe (Node /\ Boolean)
    canBeVisitedAgain node@(Big _) = Just (node /\ hasUsedJoker)
    canBeVisitedAgain node@(Small name) = case (hasUsedJoker /\ S.member node visitedSet) of
      (joker /\ false) -> Just (node /\ joker)
      (false /\ true) | name /= "start" -> Just (node /\ true)
      (false /\ true) -> Nothing
      (true /\ true) -> Nothing

day = makeDayWithCommonPart parse (Right <<< buildCave)
    (Right <<< show <<< countPaths true)
    (Right <<< show <<< countPaths false)

