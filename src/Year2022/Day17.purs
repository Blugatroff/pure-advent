module Year2022.Day17 (partOne, partTwo) where

import MeLude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (State, evalState, get, gets, modify, modify_)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.HashSet as HS
import Data.Hashable (class Hashable, hash)
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Map as M
import Data.NonEmpty ((:|))
import Data.Set as S
import Data.String.CodeUnits (fromCharArray, toCharArray) as String
import JS.BigInt (BigInt, fromInt, pow)
import Util (dedup, fromBigInt, tailRec0)

data Pos = Pos { x :: Int, y :: Int }

instance hashablePos :: Hashable Pos where
  hash (Pos p) = hash p

xPos :: Pos -> Int
xPos (Pos { x }) = x

yPos :: Pos -> Int
yPos (Pos { y }) = y

derive instance eqPos :: Eq Pos

instance showPos :: Show Pos where
  show (Pos { x, y }) = "(Pos " <> show x <> " " <> show y <> ")"

instance Ord Pos where
  compare (Pos l) (Pos r) = case compare l.y r.y of
    EQ -> compare l.x r.x
    r -> r

instance semiringPos :: Semiring Pos where
  add (Pos l) (Pos r) = Pos { x: l.x + r.x, y: l.y + r.y }
  zero = Pos { x: 0, y: 0 }
  mul (Pos l) (Pos r) = Pos { x: l.x * r.x, y: l.y * r.y }
  one = Pos { x: 1, y: 1 }

instance ringPos :: Ring Pos where
  sub (Pos l) (Pos r) = Pos { x: l.x - r.x, y: l.y - r.y }

data Shape = Shape String (HS.HashSet Pos)

instance eqShape :: Eq Shape where
  eq (Shape l _) (Shape r _) = l == r

instance ordShape :: Ord Shape where
  compare (Shape l _) (Shape r _) = compare l r

instance showShape :: Show Shape where
  show (Shape name _) = name

data Jet = LeftJet | RightJet

data Jets = Jets Int (NonEmptyArray Jet)

takeJet :: Jets -> Jet /\ Jets
takeJet (Jets i jets) = case NEArray.index jets i of
  Just jet -> jet /\ Jets (i + 1) jets
  Nothing -> takeJet (Jets 0 jets)

jetIndex :: Jets -> Int
jetIndex (Jets i _) = i

verticalLineShape :: Shape
verticalLineShape = Shape "VerticalLine" $ HS.fromArray $ (\y -> Pos { y, x: 0 }) <$> Array.range 0 3

horizontalLineShape :: Shape
horizontalLineShape = Shape "HorizontalLine" $ HS.fromArray $ (\x -> Pos { x, y: 0 }) <$> Array.range 0 3

crossShape :: Shape
crossShape = Shape "Cross" $ HS.fromArray $ ((\x -> Pos { x, y: 1 }) <$> Array.range 0 2) <> ((\y -> Pos { x: 1, y }) <$> Array.range 0 2)

lShape :: Shape
lShape = Shape "L" $ HS.fromArray $ ((\x -> Pos { x, y: 0 }) <$> Array.range 0 2) <> ((\y -> Pos { x: 2, y }) <$> Array.range 0 2)

squareShape :: Shape
squareShape = Shape "Square" $ HS.fromArray [ Pos { x: 0, y: 0 }, Pos { x: 1, y: 0 }, Pos { x: 0, y: 1 }, Pos { x: 1, y: 1 } ]

shapes :: LazyList.List Shape
shapes = LazyList.cycle $ LazyList.fromFoldable [ horizontalLineShape, crossShape, lShape, verticalLineShape, squareShape ]

newtype Chamber = Chamber (S.Set Pos)

emptyChamber :: Chamber
emptyChamber = Chamber S.empty

modifyChamber :: (S.Set Pos -> S.Set Pos) -> Chamber -> Chamber
modifyChamber f (Chamber s) = Chamber $ f s

instance showChamber :: Show Chamber where
  show (Chamber blocks) =
    Array.intercalate "\n" $ Array.reverse (Array.range 0 maxY)
      <#> \y -> String.fromCharArray
        $ (Array.range 0 6)
        <#> \x -> if S.member (Pos { x, y }) blocks then '#' else '.'
    where
    maxY = (S.toUnfoldable blocks :: Array Pos) <#> yPos # maximum # fromMaybe 0

type TetrisState = { chamber :: Chamber, shapes :: LazyList.List Shape, count :: Int, jets :: Jets }

mapStateChamber :: (Chamber -> Chamber) -> TetrisState -> TetrisState
mapStateChamber f state@{ chamber } = state { chamber = f chamber }

mapStateJets :: (Jets -> Jets) -> TetrisState -> TetrisState
mapStateJets f state@{ jets } = state { jets = f jets }

mapStateShapes :: (LazyList.List Shape -> LazyList.List Shape) -> TetrisState -> TetrisState
mapStateShapes f state@{ shapes } = state { shapes = f shapes }

mapStateCount :: (Int -> Int) -> TetrisState -> TetrisState
mapStateCount f state@{ count } = state { count = f count }

incrementCount :: State TetrisState Unit
incrementCount = modify_ $ mapStateCount (add 1)

spawnShape :: Pos -> Shape -> State TetrisState Unit
spawnShape pos (Shape _ blocks) =
  HS.toArray blocks
    <#> (add pos)
    # Array.filter (\(Pos { x, y }) -> y >= 0 && x >= 0 && x <= 6)
    # traverse_ (modify <<< mapStateChamber <<< modifyChamber <<< S.insert)

collisionWith :: Pos -> Shape -> Chamber -> Boolean
collisionWith pos (Shape _ shape) (Chamber blocks) = HS.toArray shape <#> (add pos) # any colliding
  where
  colliding pos@(Pos { x, y }) = x < 0 || x > 6 || y < 0 || S.member pos blocks

takeStateJet :: State TetrisState Jet
takeStateJet = do
  jet /\ jets <- gets $ takeJet <<< _.jets
  modify_ (mapStateJets $ const jets) $> jet

peekShape :: State TetrisState Shape
peekShape = gets _.shapes <#> LazyList.head >>> case _ of
  Just shape -> shape
  _ -> lShape

takeShape :: State TetrisState Shape
takeShape = gets _.shapes >>= LazyList.uncons >>> case _ of
  Just { head: shape, tail: shapes } -> modify_ (mapStateShapes $ const shapes) $> shape
  _ -> pure lShape

dropShape :: Shape -> Pos -> State TetrisState Boolean
dropShape shape = tailRecM go
  where
  go :: Pos -> State TetrisState (Step Pos Boolean)
  go pos@(Pos { x, y }) = do
    chamber <- gets _.chamber
    if collisionWith pos shape chamber then pure $ Done false
    else do
      jet <- takeStateJet
      let
        testX =
          ( case jet of
              LeftJet -> x - 1
              RightJet -> x + 1
          )
        newX = if collisionWith (Pos { x: testX, y }) shape chamber then x else testX
      if collisionWith (Pos { x: newX, y: y - 1 }) shape chamber then do
        incrementCount
        Done true <$ spawnShape (Pos { x: newX, y }) shape
      else pure $ Loop $ Pos { x: newX, y: y - 1 }

highestBlock :: Chamber -> Int
highestBlock (Chamber blocks) = case S.findMax blocks of
  Just max -> 1 + yPos max
  Nothing -> 0

parse :: String -> Jets
parse = Jets 0 <<< fromMaybe (NEArray.fromNonEmpty (LeftJet :| [])) <<< NEArray.fromArray <<< Array.mapMaybe f <<< String.toCharArray
  where
  f '>' = Just RightJet
  f '<' = Just LeftJet
  f _ = Nothing

spawnAndDropShape :: State TetrisState Unit
spawnAndDropShape = do
  shape <- takeShape
  hightest <- gets $ highestBlock <<< _.chamber
  void $ dropShape shape $ Pos { x: 2, y: hightest + 3 }

solvePartOne :: Int -> State TetrisState Int
solvePartOne n = do
  sequence_ $ Array.replicate n spawnAndDropShape
  gets $ highestBlock <<< _.chamber

newtype Seal = Seal (S.Set Pos)

derive instance eqSeal :: Eq Seal
derive instance ordSeal :: Ord Seal

instance Show Seal where
  show (Seal seal) =
    Array.intercalate "\n"
      $ Array.range maxY minY
      <#> \y ->
        String.fromCharArray $ Array.range minX maxX <#> \x ->
          if S.member (Pos { x, y }) seal then '#' else '.'
    where
    minX = (S.toUnfoldable seal :: Array Pos) <#> xPos # minimum # fromMaybe 0
    maxX = (S.toUnfoldable seal :: Array Pos) <#> xPos # maximum # fromMaybe 0
    minY = (S.toUnfoldable seal :: Array Pos) <#> yPos # minimum # fromMaybe 0
    maxY = (S.toUnfoldable seal :: Array Pos) <#> yPos # maximum # fromMaybe 0

normalizeSeal :: Seal -> Seal
normalizeSeal (Seal seal) = Seal $ S.fromFoldable $ sub (Pos { x: minX, y: minY }) <$> (S.toUnfoldable seal :: Array Pos)
  where
  minX = (S.toUnfoldable seal :: Array Pos) <#> xPos # minimum # fromMaybe 0
  minY = (S.toUnfoldable seal :: Array Pos) <#> yPos # minimum # fromMaybe 0

flood :: Chamber -> Maybe Seal
flood chamber@(Chamber blocks) =
  if eq 7 $ Array.length $ dedup $ xPos <$> (S.toUnfoldable seal :: Array Pos) then Just $ normalizeSeal $ Seal seal
  else Nothing
  where
  highest = highestBlock chamber

  seal = flow (Pos { x: 0, y: highest })

  flow :: Pos -> S.Set Pos
  flow pos = go (List.singleton pos) S.empty S.empty
    where
    go :: List Pos -> S.Set Pos -> S.Set Pos -> S.Set Pos
    go List.Nil _ seal = seal
    go (Pos { x, y } : rest) visited seal | y < 0 || y > highest || x < 0 || x > 6 = go rest visited seal
    go (pos : rest) visited seal | S.member pos seal = go rest visited seal
    go (pos : rest) visited seal | S.member pos visited = go rest visited seal
    go (pos : rest) visited seal | S.member pos blocks = go rest (S.insert pos visited) (S.insert pos seal)
    go (pos@(Pos { x, y }) : rest) visited seal = go
      ( Pos { x: x - 1, y }
          : Pos { x: x + 1, y }
          : Pos { x, y: y + 1 }
          : Pos { x, y: y - 1 }
          : rest
      )
      (S.insert pos visited)
      seal

untilSeal :: State TetrisState Seal
untilSeal = tailRec0 do
  seal <- gets $ flood <<< _.chamber
  spawnAndDropShape
  pure $ maybe (Loop unit) Done seal

newtype Match = Match { seal :: Seal, shape :: Shape, jetIndex :: Int, state :: TetrisState }

instance eqMatch :: Eq Match where
  eq (Match l) (Match r) = l.jetIndex == r.jetIndex && l.seal == r.seal && l.shape == r.shape

instance ordMatch :: Ord Match where
  compare (Match l) (Match r) = case compare l.jetIndex r.jetIndex of
    EQ -> case compare l.shape r.shape of
      EQ -> compare l.seal r.seal
      r -> r
    r -> r

findSealPair :: State TetrisState Match
findSealPair = tailRecM go M.empty
  where
  go previousSeals = do
    void spawnAndDropShape
    nextSeal <- untilSeal
    nextShape <- peekShape
    state <- get
    let nextJetIndex = jetIndex state.jets
    let next = Match { seal: nextSeal, shape: nextShape, jetIndex: nextJetIndex, state }
    pure $ case M.lookup next previousSeals of
      Just p -> Done p
      Nothing -> Loop $ M.insert next next previousSeals

solvePartTwo :: BigInt -> State TetrisState String
solvePartTwo n = do
  jets <- gets _.jets
  Match { state: previousState } <- findSealPair
  let h1 = fromInt $ highestBlock $ previousState.chamber
  state <- get
  let h2 = fromInt $ highestBlock $ state.chamber
  let hd = h2 - h1
  let c1 = fromInt $ previousState.count
  let c2 = fromInt $ state.count
  let cd = c2 - c1

  let repeats = (n - c1) `div` cd
  let skippedSteps = repeats * cd
  let skippedHeight = repeats * hd
  let remainingSteps = n - skippedSteps - c1
  let remainingAndFirstSteps = c1 + remainingSteps

  let h = evalState (solvePartOne $ fromMaybe 0 $ fromBigInt remainingAndFirstSteps) $ { chamber: emptyChamber, shapes, count: 0, jets }

  pure $ show $ fromInt h + skippedHeight

partOne :: String -> String |? String
partOne = Right <<< show <<< evalState (solvePartOne 2022) <<< (\jets -> { chamber: emptyChamber, shapes, count: 0, jets }) <<< parse

trillion = fromInt 10 `pow` fromInt 12

partTwo :: String -> String |? String
partTwo = Right <<< evalState (solvePartTwo trillion) <<< (\jets -> { chamber: emptyChamber, shapes, count: 0, jets }) <<< parse
