module Year2021.Day18 (partOne, partTwo) where

import MeLude

import Control.Monad.State (class MonadState, StateT, evalStateT, get, lift, put)
import Data.Array as Array
import Data.CodePoint.Unicode (isDecDigit)
import Data.Foldable (maximumBy)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Ord (abs)
import Data.Show.Generic (genericShow)
import Data.String (codePointFromChar)
import Data.String as String
import Data.Tuple (uncurry)
import Util (mapFst, reduceL, sign, splitStringOnce)
import Util as Util

data Element = Number Int | Pair Element Element

derive instance eqElement :: Eq Element
derive instance genericElement :: Generic Element _
instance showElement :: Show Element where
  show e = genericShow e

parseChar :: Char -> StateT (List Char) (Either String) Char
parseChar char =
  get >>= case _ of
    Nil -> lift $ Left $ "expected " <> fromCharArray [ char ] <> " found empty"
    (c : rest)
      | c == char -> do
          put rest
          pure c
      | otherwise -> lift $ Left $ "expected " <> fromCharArray [ char ] <> " found: " <> fromCharArray [ c ]

throwInState :: forall s e a. e -> StateT s (Either e) a
throwInState error = lift $ Left error

isCharDecDigit = codePointFromChar >>> isDecDigit

parseInt :: StateT (List Char) (Either String) Int
parseInt = get >>= case _ of
  Nil -> throwInState "expected number found ''"
  (first : rest) | isCharDecDigit first -> do
    number <- takeFromStateWhile isCharDecDigit
    lift $ Util.parseInt (fromCharArray $ Array.fromFoldable number)
  rest -> throwInState $ "expected number found: " <> (fromCharArray $ Array.fromFoldable rest)

takeFromStateWhile :: forall a m. MonadState (List a) m => (a -> Boolean) -> m (List a)
takeFromStateWhile predicate =
  get >>= case _ of
    Nil -> pure Nil
    (next : rest)
      | predicate next -> do
          put rest
          takeFromStateWhile predicate <#> (next : _)
    _ -> pure Nil

parseElement :: StateT (List Char) (Either String) Element
parseElement =
  get >>= case _ of
    Nil -> throwInState "Expected element found empty"
    ('[' : rest) -> do
      put rest
      elem1 <- parseElement
      _ <- parseChar ','
      elem2 <- parseElement
      _ <- parseChar ']'
      pure $ Pair elem1 elem2
    (first : rest)
      | isCharDecDigit first -> Number <$> parseInt
      | otherwise -> throwInState $ "Expected element found: " <> fromCharArray (Array.fromFoldable (first : rest))

parse :: String -> String |? Array Element
parse input = traverse (evalStateT parseElement)
  $ Array.filter (not <<< List.null)
  $ map (List.fromFoldable <<< toCharArray)
  $ String.split (String.Pattern "\n") input

data Location = Here | LeftSide Location | RightSide Location

derive instance eqLocation :: Eq Location

instance ordLocation :: Ord Location where
  compare Here Here = EQ
  compare (LeftSide _) Here = LT
  compare (RightSide _) Here = GT
  compare Here (LeftSide _) = GT
  compare Here (RightSide _) = LT
  compare (LeftSide left) (RightSide right) = compare left right
  compare (RightSide right) (LeftSide left) = compare right left
  compare (LeftSide a) (LeftSide b) = compare a b
  compare (RightSide a) (RightSide b) = compare a b

inDepth :: Int -> Element -> Array (Location /\ Element)
inDepth 0 element = [ Here /\ element ]
inDepth depth (Number _) = []
inDepth depth (Pair left right) =
  (mapFst LeftSide <$> inDepth (depth - 1) left)
    <> (mapFst RightSide <$> inDepth (depth - 1) right)

getElement :: Location -> Element -> Maybe Element
getElement Here element = Just element
getElement (LeftSide loc) (Number n) = Nothing
getElement (RightSide loc) (Number n) = Nothing
getElement (LeftSide loc) (Pair left right) = getElement loc left
getElement (RightSide loc) (Pair left right) = getElement loc right

replaceElement :: Location -> (Element -> Element) -> Element -> (Element /\ Maybe Element)
replaceElement Here new tree = new tree /\ Just tree
replaceElement (RightSide loc) new (Number n) = Number n /\ Nothing
replaceElement (LeftSide loc) new (Number n) = Number n /\ Nothing
replaceElement (LeftSide loc) new (Pair left right) = replaceElement loc new left # mapFst (_ `Pair` right)
replaceElement (RightSide loc) new (Pair left right) = replaceElement loc new right # mapFst (Pair left)

elementsWithLocation :: Element -> Array (Location /\ Element)
elementsWithLocation el@(Number n) = [ Here /\ el ]
elementsWithLocation el@(Pair left right) =
  [ Here /\ el ]
    <> (mapFst LeftSide <$> elementsWithLocation left)
    <> (mapFst RightSide <$> elementsWithLocation right)

isPair :: Element -> Boolean
isPair (Pair left right) = true
isPair (Number n) = false

isNumber :: Element -> Boolean
isNumber (Number n) = true
isNumber pair = false

explode :: Location -> Element -> Element
explode location element = case explodingPair of
  Just (Pair (Number left) (Number right)) ->
    replaceExploding $ addLeftValue left $ addRightValue right element
  nonRegularNumberPair -> element
  where
  add :: Int -> Element -> Element
  add v (Number n) = Number $ n + v
  add v el = el

  explodingPair = getElement location element

  replaceExploding :: Element -> Element
  replaceExploding = fst <<< replaceElement location (const (Number 0))

  addRightValue :: Int -> Element -> Element
  addRightValue value element =
    firstRight
      <#> (\firstRight -> replaceElement firstRight (add value) element # fst)
      # fromMaybe element

  addLeftValue :: Int -> Element -> Element
  addLeftValue value element =
    firstLeft
      <#> (\firstLeft -> replaceElement firstLeft (add value) element # fst)
      # fromMaybe element

  firstLeft = firstNumberToTheLeft location element
  firstRight = firstNumberToTheRight location element

  firstNumberToTheLeft :: Location -> Element -> Maybe Location
  firstNumberToTheLeft _ (Number n) = Nothing
  firstNumberToTheLeft Here (Pair left right) = Nothing
  firstNumberToTheLeft (RightSide loc) (Pair (Number n) right) =
    (firstNumberToTheLeft loc right <#> RightSide) <|> Just (LeftSide Here)
  firstNumberToTheLeft (RightSide loc) (Pair left right) =
    (firstNumberToTheLeft loc right <#> RightSide) <|> lastNumberInLeftElement
    where
    lastNumberInLeftElement :: Maybe Location
    lastNumberInLeftElement =
      elementsWithLocation left # Array.filter (isNumber <<< snd)
        <#> fst
        # Array.last
        <#> LeftSide
  firstNumberToTheLeft (LeftSide loc) (Pair left right) =
    firstNumberToTheLeft loc left <#> LeftSide

  firstNumberToTheRight :: Location -> Element -> Maybe Location
  firstNumberToTheRight _ (Number n) = Nothing
  firstNumberToTheRight Here (Pair left right) = Nothing
  firstNumberToTheRight (LeftSide loc) (Pair left (Number n)) =
    (firstNumberToTheRight loc left <#> LeftSide) <|> Just (RightSide Here)
  firstNumberToTheRight (LeftSide loc) (Pair left right) =
    (firstNumberToTheRight loc left <#> LeftSide) <|> firstNumberInRightElement
    where
    firstNumberInRightElement :: Maybe Location
    firstNumberInRightElement =
      elementsWithLocation right # Array.filter (isNumber <<< snd)
        <#> fst
        # Array.head
        <#> RightSide
  firstNumberToTheRight (RightSide loc) (Pair left right) =
    firstNumberToTheRight loc right <#> RightSide

findBigRegularNumbers :: Element -> Array Location
findBigRegularNumbers = map fst <<< Array.filter (test <<< snd) <<< elementsWithLocation
  where
    test :: Element -> Boolean
    test (Number n) | n >= 10 = true
    test notABigNumber = false

splitElement :: Location -> Element -> Element
splitElement location = fst <<< replaceElement location splitter
  where
    splitter :: Element -> Element
    splitter (Number n) = Pair (Number (n `div` 2)) (Number (n - (n `div` 2)))
    splitter element = element

reduceElement :: Element -> Element
reduceElement element = maybe element reduceElement (step element)
  where
    step :: Element -> Maybe Element
    step element = exploded <|> splitted
      where
        exploding = Array.head (map fst $ Array.filter (isPair <<< snd) $ inDepth 4 element)
        exploded = exploding <#> (_ `explode` element)

        toSplit = Array.head $ findBigRegularNumbers element
        splitted = toSplit <#> (_ `splitElement` element)

addElements :: Element -> Element -> Element
addElements a b = reduceElement $ Pair a b

magnitude :: Element -> Int
magnitude (Number n) = n
magnitude (Pair left right) = 3 * magnitude left + 2 * magnitude right

solvePartOne :: Array Element -> Int
solvePartOne = List.fromFoldable >>> reduceL addElements >>> maybe 0 magnitude

everyPair :: forall a. Array a -> Array (a /\ a)
everyPair elems = elems >>= \elem -> elems <#> (elem /\ _)

solvePartTwo :: Array Element -> Int
solvePartTwo =
  everyPair
    >>> map (magnitude <<< uncurry addElements)
    >>> maximum
    >>> fromMaybe 0

partOne = parse >>> map solvePartOne >>> map show
partTwo = parse >>> map solvePartTwo >>> map show

