module Util
  ( TransparentString(..)
  , bench
  , bindMaybes
  , chunks
  , dedup
  , filterString
  , fromBigInt
  , indexed
  , lines
  , mapFst
  , mapSnd
  , mapTrace
  , mapWithPrevious
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
  , windows
  , windows2
  ) where

import MeLude

import Control.Monad.Rec.Class (class MonadRec, Step, tailRecM)
import Data.Array as Array
import Data.Int as Int
import Data.List as List
import Data.Set as S
import Data.String as String
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Js.BigInt.BigInt (BigInt)
import Parsing as Parsing
import Parsing.Combinators as ParsingCombinators
import Parsing.String as ParsingString

parseInt ∷ String → Either String Int
parseInt s = case Int.fromString s of
  Nothing -> Left $ "failed to parse '" <> s <> "'"
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

pairs :: forall a. List a -> List (a /\ a)
pairs list = chunks 2 list >>= case _ of
  (a : b : List.Nil) -> List.singleton $ a /\ b
  _ -> List.Nil

windows :: forall a. Int -> List a -> List (List a)
windows _ Nil = Nil
windows size list = List.take size list : windows size (List.drop 1 list)

windows2 :: forall a. List a -> List (a /\ a)
windows2 l = List.zip l $ List.drop 1 l

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

space = void $ ParsingString.char ' '

tailRec0 :: forall m a. MonadRec m => m (Step Unit a) -> m a
tailRec0 m = tailRecM (const m) unit
