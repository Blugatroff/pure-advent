module Data.Pos (Pos(..), x, y) where

import Prelude

import Data.Hashable (class Hashable)
import Data.Int.Bits (shl, or)
import Data.Primitive (class PrimitiveKey)

data Pos = Pos Int Int

instance hashablePos :: Hashable Pos where
  hash (Pos x y) = x * 31 + y

instance eqPos :: Eq Pos where
  eq (Pos x1 y1) (Pos x2 y2) = x1 == x2 && y1 == y2

instance ordPos :: Ord Pos where
  compare (Pos x1 y1) (Pos x2 y2) = case compare x1 x2 of
    LT -> LT
    GT -> GT
    EQ -> compare y1 y2

instance showPos :: Show Pos where
  show (Pos x y) = "(Pos " <> show x <> " " <> show y <> ")"

instance primitiveKeyPos :: PrimitiveKey Pos Int where
  primitiveKey (Pos x y) = (x `shl` 16) `or` y

x :: Pos -> Int
x (Pos x _) = x

y :: Pos -> Int
y (Pos _ y) = y

