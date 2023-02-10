module Util
  ( TransparentString(..)
  , bench
  , bindMaybes
  , chunks
  , dedup
  , filterString
  , indexed
  , lines
  , mapFst
  , mapSnd
  , mapTrace
  , mapWithPrevious
  , pairs
  , parseInt
  , reduceL
  , reduceR
  , sign
  , split
  , splitAt
  , splitOnce
  , splitStringOnce
  , trace
  , windows
  , windows2
  )
  where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Int as Int
import Data.List (List(..), reverse, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error, error)
import Effect.Unsafe (unsafePerformEffect)

parseInt :: String -> Either Error Int
parseInt s = case Int.fromString s of
  Nothing -> Left $ error $ s <> " is not an int"
  Just n -> Right n

trace :: forall a. Show a => String -> a -> a
trace = flip mapTrace identity

mapTrace :: forall a b. Show b => String -> (a -> b) -> a -> a
mapTrace label map value = unsafePerformEffect do
  Console.error $ label <> ": " <> show (map value)
  pure value

split :: forall a. Eq a => a -> List a -> List (List a)
split _ Nil = Nil
split del list = f $ List.span (notEq del) list
  where
  f { init: Nil, rest: Nil } = Nil
  f { init: Nil, rest: (_ : rest) } = Nil : split del rest
  f { init: seg, rest: Nil } = List.singleton seg
  f { init: seg, rest: (_ : rest) } = seg : split del rest

splitOnce :: forall a. Eq a => a -> List a -> Maybe (Tuple (List a) (List a))
splitOnce del list = case split del list of
  (a : b : Nil) -> Just $ Tuple a b
  _ -> Nothing

splitStringOnce :: String -> String -> Maybe (Tuple String String)
splitStringOnce del s = case String.split (String.Pattern del) s of
  [ a, b ] -> Just $ Tuple a b
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
  Tuple chunk rest -> chunk : chunks size rest

splitAt :: forall a. Int -> List a -> Tuple (List a) (List a)
splitAt index list = Tuple (List.take index list) (List.drop index list)

indexed :: forall index f a. FunctorWithIndex index f => f a -> f (Tuple index a)
indexed = mapWithIndex Tuple

sign :: Int -> Int
sign 0 = 0
sign n | n < 0 = -1
sign _ = 1

mapWithPrevious :: forall a b. (b -> a -> b) -> b -> List a -> List b
mapWithPrevious f init list = reverse $ inner init Nil list
  where
  inner :: b -> List b -> List a -> List b
  inner _ acc Nil = acc
  inner previous acc (x : xs) = let v = f previous x in inner v (v : acc) xs

mapFst :: forall a b c. (a -> c) -> Tuple a b -> Tuple c b
mapFst f (Tuple a b) = Tuple (f a) b

mapSnd :: forall a b c. (b -> c) -> Tuple a b -> Tuple a c
mapSnd f (Tuple a b) = Tuple a (f b)

dedup :: forall fi fo v. Foldable fi => Ord v => Unfoldable fo => fi v -> fo v
dedup = S.fromFoldable >>> S.toUnfoldable

pairs :: forall a. List a -> List (Tuple a a)
pairs list = chunks 2 list >>= case _ of
  (a : b : List.Nil) -> List.singleton $ Tuple a b
  _ -> List.Nil

windows :: forall a. Int -> List a -> List (List a)
windows _ Nil = Nil
windows size list = List.take size list : windows size (List.drop 1 list)

windows2 :: forall a. List a -> List (Tuple a a)
windows2 l = List.zip l $ List.drop 1 l

foreign import performanceNow :: Effect Number

bench :: forall a b. (a -> b) -> a -> Effect (Tuple Number b)
bench f a = do
  before <- performanceNow
  b <- pure $ f a
  after <- performanceNow
  pure $ Tuple (after - before) b

filterString :: (Char -> Boolean) -> String -> String
filterString f = toCharArray >>> Array.filter f >>> fromCharArray

data TransparentString = TransparentString String

instance showTransparentString :: Show TransparentString where
  show (TransparentString s) = s

bindMaybes :: forall a. a -> List (a -> Maybe a) -> Maybe a
bindMaybes _ List.Nil = Nothing
bindMaybes a (x:xs) = case x a of
  Nothing -> Nothing
  Just a -> bindMaybes a xs
