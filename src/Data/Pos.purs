module Data.Pos (Pos(..)) where

import Prelude
import Data.Primitive (class PrimitiveKey)
import Data.Int.Bits (shl, or)

data Pos = Pos Int Int

instance eqPos :: Eq Pos where
  eq (Pos x1 y1) (Pos x2 y2) = x1 == x2 && y1 == y2

instance ordPos :: Ord Pos where
  compare (Pos x1 y1) (Pos x2 y2) = case compare x1 y1 of
    LT -> LT
    GT -> GT
    EQ -> compare x2 y2

instance showPos :: Show Pos where
  show (Pos x y) = "(Pos " <> show x <> " " <> show y <> ")"

instance primitiveKeyPos :: PrimitiveKey Pos Int where
  primitiveKey (Pos x y) = (x `shl` 16) `or` y

