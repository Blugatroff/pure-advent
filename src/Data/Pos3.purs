module Data.Pos3 (Pos3(..), x, y, z, manhattan) where

import MeLude

data Pos3 = Pos3 Int Int Int

instance eqPos3 :: Eq Pos3 where
  eq (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

instance ordPos3 :: Ord Pos3 where
  compare (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = case compare x1 x2 of
    LT -> LT
    GT -> GT
    EQ -> case compare y1 y2 of
      LT -> LT
      GT -> GT
      EQ -> compare z1 z2

instance showPos3 :: Show Pos3 where
  show (Pos3 x y z) = "(Pos3 " <> show x <> " " <> show y <> " " <> show z <> ")"

x :: Pos3 -> Int
x (Pos3 x _ _) = x

y :: Pos3 -> Int
y (Pos3 _ y _) = y

z :: Pos3 -> Int
z (Pos3 _ _ z) = z

instance semiringPos3 :: Semiring Pos3 where
  add (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = Pos3 (x1 + x2) (y1 + y2) (z1 + z2)
  zero = Pos3 0 0 0
  mul (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = Pos3 (x1 * x2) (y1 * y2) (z1 * z2)
  one = Pos3 1 1 1

instance ringPos3 :: Ring Pos3 where
  sub (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = Pos3 (x1 - x2) (y1 - y2) (z1 - z2)

manhattan :: Pos3 -> Pos3 -> Int
manhattan (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

