module Data.Range where

import MeLude

import Data.Array as Array
import Data.List as List

-- inclusive start and end
data Range a = Range a a

derive instance ordRange :: Ord a => Ord (Range a)
derive instance eqRange :: Eq a => Eq (Range a)
instance showRange :: Show a => Show (Range a) where
  show (Range x y) = show x <> ".." <> show y

infixr 6 Range as ..

new :: forall a. Ord a => a -> a -> Range a
new s e | e >= s = s .. e
new s e = e .. s

newOrdered :: forall a. Ord a => a -> a -> Maybe (Range a)
newOrdered s e | e >= s = Just (s .. e)
newOrdered _ _ = Nothing

start :: forall a. Range a -> a
start (s .. _) = s

end :: forall a. Range a -> a
end (_ .. e) = e

inRange :: forall a. Ord a => Range a -> a -> Boolean
inRange (s .. e) n = n >= s && n <= e

size :: forall a. Ring a => Ord a => Range a -> a
size (s .. e) = add one $ abs $ e - s

contains :: forall a. Ord a => Range a -> Range a -> Boolean
contains l (sr .. er) = inRange l sr && inRange l er

intersects :: forall a. Ord a => Range a -> Range a -> Boolean
intersects l@(sl .. el) r@(sr .. er) = inRange r sl || inRange r el || inRange l sr || inRange l er

intersection :: forall a. Ord a => Range a -> Range a -> Maybe (Range a)
intersection a@(as .. ae) b@(bs .. be) | intersects a b = Just $ (max as bs) .. (min ae be)
intersection _ _ = Nothing

tryMerge :: forall a. Ord a => Semiring a => Range a -> Range a -> Range a /\ Range a |? Range a
tryMerge l@(sl .. el) (sr .. er) | inRange l sr = Right $ sl .. max el er
tryMerge l@(sl .. el) (sr .. er) | inRange l er = Right $ min sl sr .. el
tryMerge (sl .. el) r@(sr .. er) | inRange r sl = Right $ sr .. max el er
tryMerge (sl .. el) r@(sr .. er) | inRange r el = Right $ min sl sr .. er
tryMerge (sl .. el) (sr .. er) | el + one == sr = Right $ sl .. er
tryMerge (sl .. el) (sr .. er) | er + one == sl = Right $ sr .. el
tryMerge l r = Left $ l /\ r

tryMergeAll :: forall a. Ord a => Semiring a => List (Range a) -> List (Range a)
tryMergeAll ranges = f $ List.sortBy (compare `on` start) ranges
  where
  f :: List (Range a) -> List (Range a)
  f List.Nil = List.Nil
  f (r : List.Nil) = List.singleton r
  f (a : b : rest) = case tryMerge a b of
    Left (a /\ b) -> a : f (b : rest)
    Right r -> f $ r : rest

points :: Range Int -> Array Int
points (Range s e) = Array.range s e

