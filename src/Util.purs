module Util
  ( TransparentString(..)
  , bench
  , bindMaybes
  , chunks
  , dedup
  , dedupCount
  , filterString
  , fromBigInt
  , indexed
  , lines
  , mapFst
  , mapSnd
  , traceWith 
  , mapWithPrevious
  , mergeEither
  , newline
  , pairs
  , parseInt
  , reduceL
  , reduceR
  , sign
  , space
  , split
  , splitAt
  , splitOnce
  , splitStringOnce
  , tailRec0
  , trace
  , traceRuntime
  , tuplePermutations
  , windows
  , windows2
  , windowsNonEmpty
  , mapSum
  , median
  , toCharUnfoldable
  , dedupCountPrimitive
  , repeatM
  , nonEmptyLines
  , minimumOrZero
  , maximumOrZero
  ) where

import MeLude

import Control.Monad.Rec.Class (class MonadRec, Step, tailRecM)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.DateTime.Instant as Instant
import Data.Int as Int
import Data.List as List
import Data.Map as M
import Data.NonEmpty (NonEmpty(..))
import Data.Primitive (class PrimitiveKey, primitiveKey)
import Data.Set as S
import Data.String as String
import Data.Map.Native.ST as NativeMapST
import Effect.Console as Console
import Effect.Now as Now
import Effect.Unsafe (unsafePerformEffect)
import JS.BigInt (BigInt)
import Parsing (ParserT)
import Parsing as Parsing
import Parsing.Combinators as ParsingCombinators
import Parsing.String as ParsingString

parseInt ∷ String → Either String Int
parseInt s = case Int.fromString s of
  Nothing -> Left $ "failed to parse '" <> s <> "'"
  Just n -> Right n

trace :: forall a. Show a => String -> a -> a
trace = flip traceWith show

traceWith :: forall a b. String -> (a -> String) -> a -> a
traceWith label map value = unsafePerformEffect do
  Console.error $ label <> ": " <> map value
  pure value

traceRuntime :: forall a b. String -> (a -> b) -> (a -> b)
traceRuntime label f a = unsafePerformEffect do
  start <- liftEffect $ unwrap <<< Instant.unInstant <$> Now.now
  result <- pure $ f a
  end <- liftEffect $ unwrap <<< Instant.unInstant <$> Now.now
  let duration = end - start
  Console.error $ label <> ": " <> show duration
  pure $ result

split :: forall a. Eq a => a -> List a -> List (List a)
split _ Nil = Nil
split del list = f $ List.span (notEq del) list
  where
  f { init: Nil, rest: Nil } = Nil
  f { init: Nil, rest: (_ : rest) } = Nil : split del rest
  f { init: seg, rest: Nil } = List.singleton seg
  f { init: seg, rest: (_ : rest) } = seg : split del rest

splitOnce :: forall a. Eq a => a -> List a -> Maybe (List a /\ List a)
splitOnce del list = case split del list of
  (a : b : Nil) -> Just $ a /\ b
  _ -> Nothing

splitStringOnce :: String -> String -> Maybe (String /\ String)
splitStringOnce del s = case String.split (String.Pattern del) s of
  [ a, b ] -> Just $ a /\ b
  _ -> Nothing

lines :: String -> Array String
lines s = String.split (String.Pattern "\n") s <#> String.replace (String.Pattern "\r") (String.Replacement "")

reduceR :: forall a. (a -> a -> a) -> List a -> Maybe a
reduceR _ Nil = Nothing
reduceR fold (first : rest) = Just $ List.foldr fold first rest

reduceL :: forall a. (a -> a -> a) -> List a -> Maybe a
reduceL _ Nil = Nothing
reduceL fold (first : rest) = Just $ List.foldl fold first rest

chunks :: forall a. Int -> List a -> List (List a)
chunks _ Nil = Nil
chunks size list = case splitAt size list of
  chunk /\ rest -> chunk : chunks size rest

splitAt :: forall a. Int -> List a -> List a /\ List a
splitAt index list = List.take index list /\ List.drop index list

indexed :: forall index f a. FunctorWithIndex index f => f a -> f (index /\ a)
indexed = mapWithIndex (/\)

sign :: Int -> Int
sign 0 = 0
sign n | n < 0 = -1
sign _ = 1

mapWithPrevious :: forall a b. (b -> a -> b) -> b -> List a -> List b
mapWithPrevious f init list = List.reverse $ inner init Nil list
  where
  inner :: b -> List b -> List a -> List b
  inner _ acc Nil = acc
  inner previous acc (x : xs) = let v = f previous x in inner v (v : acc) xs

mapFst :: forall a b c. (a -> c) -> a /\ b -> c /\ b
mapFst f (a /\ b) = f a /\ b

mapSnd :: forall a b c. (b -> c) -> a /\ b -> a /\ c
mapSnd f (a /\ b) = a /\ f b

dedup :: forall fi fo v. Foldable fi => Ord v => Unfoldable fo => fi v -> fo v
dedup = S.fromFoldable >>> S.toUnfoldable

dedupCount :: forall fi fo v. Foldable fi => Ord v => Unfoldable fo => fi v -> fo (v /\ Int)
dedupCount = M.toUnfoldable <<< foldl f M.empty
  where
  f map k = M.insertWith add k 1 map

dedupCountPrimitive :: forall v prim. PrimitiveKey v prim => Array v -> Array (v /\ Int)
dedupCountPrimitive f = ST.run do
  m <- NativeMapST.empty
  ST.foreach f \v -> do
    let key = primitiveKey v
    found <- NativeMapST.lookup key m
    case found of
      Nothing -> NativeMapST.insert key (v /\ 1) m
      Just (_ /\ c) -> NativeMapST.insert key (v /\ (c + 1)) m
  map snd <$> NativeMapST.entries m

pairs :: forall a. List a -> List (a /\ a)
pairs list = chunks 2 list >>= case _ of
  (a : b : List.Nil) -> List.singleton $ a /\ b
  _ -> List.Nil

windows :: forall a. Int -> List a -> List (List a)
windows size = go List.Nil
  where
  go :: List (List a) -> List a -> List (List a)
  go accum Nil = List.reverse accum
  go accum list = go (List.take size list : accum) (List.drop 1 list)

windows2 :: forall a. List a -> List (a /\ a)
windows2 l = List.zip l $ List.drop 1 l

windowsNonEmpty :: forall a. Int -> NonEmpty List a -> NonEmpty List (List a)
windowsNonEmpty _ (NonEmpty head List.Nil) = NonEmpty (List.singleton head) List.Nil
windowsNonEmpty size (NonEmpty head tail) = NonEmpty (head : List.take (size - 1) tail) (windows size tail)

foreign import performanceNow :: Effect Number

bench :: forall a b. (a -> b) -> a -> Effect (Number /\ b)
bench f a = do
  before <- performanceNow
  b <- pure $ f a
  after <- performanceNow
  pure $ (after - before) /\ b

filterString :: (Char -> Boolean) -> String -> String
filterString f = toCharArray >>> Array.filter f >>> fromCharArray

data TransparentString = TransparentString String

instance showTransparentString :: Show TransparentString where
  show (TransparentString s) = s

foreign import fromBigIntImpl :: (forall a. Maybe a) -> (forall a. a -> Maybe a) -> BigInt -> Maybe Int

fromBigInt :: BigInt -> Maybe Int
fromBigInt = fromBigIntImpl Nothing Just

bindMaybes :: forall a. List (a -> Maybe a) -> a -> Maybe a
bindMaybes List.Nil a = Just a
bindMaybes (f : fs) a = case f a of
  Nothing -> Nothing
  Just a -> bindMaybes fs a

newline ∷ forall m. Parsing.ParserT String m Unit
newline = ParsingCombinators.choice [ void $ ParsingString.char '\n', void $ ParsingString.string "\r\n" ]

space ∷ ∀ m. ParserT String m Unit
space = void $ ParsingString.char ' '

tailRec0 :: forall m a. MonadRec m => m (Step Unit a) -> m a
tailRec0 m = tailRecM (const m) unit

repeatM :: forall m. Applicative m => Int -> m Unit -> m Unit
repeatM n m = sequence_ $ Array.replicate n m

tuplePermutations :: forall f a. Foldable f => f a -> Array (a /\ a)
tuplePermutations =
  Array.fromFoldable >>> \items ->
    indexed items >>= \(i /\ item1) ->
      let
        f = \(j /\ item2) -> if i /= j && j <= i then Just (item1 /\ item2) else Nothing
      in
        Array.mapMaybe f $ indexed items

mergeEither :: forall a. Either a a -> a
mergeEither (Left a) = a
mergeEither (Right a) = a

mapSum :: forall f a b. Semiring b => Functor f => Foldable f => (a -> b) -> f a -> b
mapSum = map (map sum) map

median :: forall a. Ord a => Array a -> Maybe a
median elems = Array.index (Array.sort elems) (Array.length elems `div` 2)

toCharUnfoldable :: forall f. Unfoldable f => String -> f Char
toCharUnfoldable = toCharArray >>> Array.toUnfoldable

nonEmptyLines :: String -> Array String
nonEmptyLines = lines
  >>> map String.trim
  >>> Array.filter (not <<< String.null)

minimumOrZero :: forall f a. Foldable f => Ord a => Semiring a => f a -> a
minimumOrZero = fromMaybe zero <<< minimum

maximumOrZero :: forall f a. Foldable f => Ord a => Semiring a => f a -> a
maximumOrZero = fromMaybe zero <<< maximum

