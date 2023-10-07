module Data.Pos3 (Pos3(..), x, y, z, manhattan) where

import MeLude

data Pos3 a = Pos3  a a a

instance eqPos3 :: Eq a => Eq (Pos3 a) where
  eq (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

instance ordPos3 :: Ord a => Ord (Pos3 a) where
  compare (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = case compare x1 x2 of
    LT -> LT
    GT -> GT
    EQ -> case compare y1 y2 of
      LT -> LT
      GT -> GT
      EQ -> compare z1 z2

instance showPos3 :: Show a => Show (Pos3 a) where
  show (Pos3 x y z) = "(Pos3 " <> show x <> " " <> show y <> " " <> show z <> ")"

x :: forall a. Pos3 a -> a
x (Pos3 x _ _) = x

y :: forall a. Pos3 a -> a
y (Pos3 _ y _) = y

z :: forall a. Pos3 a -> a
z (Pos3 _ _ z) = z

instance semiringPos3 :: Semiring a => Semiring (Pos3 a) where
  add (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = Pos3 (x1 + x2) (y1 + y2) (z1 + z2)
  zero = Pos3 zero zero zero
  mul (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = Pos3 (x1 * x2) (y1 * y2) (z1 * z2)
  one = Pos3 one one one

instance ringPos3 :: Ring a => Ring (Pos3 a) where
  sub (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = Pos3 (x1 - x2) (y1 - y2) (z1 - z2)

manhattan :: forall a. Ord a => Ring a => Pos3 a -> Pos3 a -> a
manhattan (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

