module Year2021.Day12 (partOne, partTwo) where

import MeLude

import Data.Array as Array
import Data.CodePoint.Unicode (isUpper)
import Data.Foldable (foldr)
import Data.List as List
import Data.Map as M
import Data.String (toCodePointArray)
import Data.String as String

data Node = Small String | Big String

derive instance eqNode :: Eq Node

instance showNode :: Show Node where
  show = show <<< nodeName

type Link = Node /\ Node

type Cave = M.Map String (List Node)

type Visited = List Node

data PathTree = Arrived | Branches (List (String /\ PathTree))

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

parse = String.split (String.Pattern "\n") >>> Array.filter (not <<< String.null) >>> traverse parseLine

addLinkToCave :: Link -> Cave -> Cave
addLinkToCave (left /\ right) = addRightToLeft <<< addLeftToRight
  where
  leftName = nodeName left
  rightName = nodeName right

  addRightToLeft cave = case M.lookup leftName cave of
    Just links -> M.insert leftName (right : links) cave
    Nothing -> M.insert leftName (right : List.Nil) cave
  addLeftToRight cave = case M.lookup rightName cave of
    Just links -> M.insert rightName (left : links) cave
    Nothing -> M.insert rightName (left : List.Nil) cave

buildCave = foldr addLinkToCave M.empty

buildPathTree :: Visited -> Node -> Node -> Cave -> String |? PathTree
buildPathTree _ from to _ | from == to = Right Arrived
buildPathTree visited from to cave = do
  neighbours <- filterNeighbours <$> lookup from
  Branches <$> traverse launchNext neighbours
  where
  lookup node = case M.lookup (nodeName node) cave of
    Just node -> Right node
    Nothing -> Left $ "failed to find node " <> nodeName node

  launchNext neighbour = buildPathTree (from : visited) neighbour to cave <#> (nodeName neighbour /\ _)

  filterNeighbours :: List Node -> List Node
  filterNeighbours neighbours = List.filter canBeVisitedAgain neighbours
    where
    canBeVisitedAgain :: Node -> Boolean
    canBeVisitedAgain (Big _) = true
    canBeVisitedAgain (Small name) = all (nodeName >>> notEq name) visited

pathTree :: Cave -> String |? PathTree
pathTree = buildPathTree List.Nil (node "start") (node "end")
  >>> map \tree -> Branches (("start" /\ tree) : List.Nil)

buildPathTreePartTwo :: Boolean -> Visited -> Node -> Node -> Cave -> String |? PathTree
buildPathTreePartTwo _ _ from to _ | from == to = Right Arrived
buildPathTreePartTwo hasUsedJoker visited from to cave = do
  neighbours <- filterNeighbours <$> lookup from
  Branches <$> traverse launchNext neighbours
  where
  lookup node = case M.lookup (nodeName node) cave of
    Just node -> Right node
    Nothing -> Left $ "failed to find node " <> nodeName node
  launchNext (neighbour /\ usedJoker) = buildPathTreePartTwo usedJoker (from : visited) neighbour to cave <#> (nodeName neighbour /\ _)

  filterNeighbours :: List Node -> List (Node /\ Boolean)
  filterNeighbours = List.mapMaybe canBeVisitedAgain

  canBeVisitedAgain :: Node -> Maybe (Node /\ Boolean)
  canBeVisitedAgain node@(Big _) = Just (node /\ hasUsedJoker)
  canBeVisitedAgain node@(Small name) = case (hasUsedJoker /\ all (nodeName >>> notEq name) visited) of
    (joker /\ true) -> Just (node /\ joker)
    (false /\ false) | name /= "start" -> Just (node /\ true)
    (false /\ false) -> Nothing
    (true /\ false) -> Nothing

pathTreePartTwo :: Cave -> String |? PathTree
pathTreePartTwo = buildPathTreePartTwo false List.Nil (node "start") (node "end")
  >>> map \tree -> Branches (("start" /\ tree) : List.Nil)

pathTreeToList :: PathTree -> List (List String)
pathTreeToList Arrived = List.singleton List.Nil
pathTreeToList (Branches branches) =
  branches >>= (\(name /\ subTree) -> pathTreeToList subTree <#> (name : _))

solvePartOne links = pathTree (buildCave links) <#> pathTreeToList >>> List.length
solvePartTwo links = pathTreePartTwo (buildCave links) <#> pathTreeToList >>> List.length

partOne input = parse input >>= solvePartOne <#> show
partTwo input = parse input >>= solvePartTwo <#> show

