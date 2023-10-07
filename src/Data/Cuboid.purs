module Data.Cuboid where

import MeLude

import Data.Pos3 (Pos3(..))
import Data.Range (Range)
import Data.Range as Range

data Cuboid a = Cuboid (Range a) (Range a) (Range a)

instance showCuboid :: Show a => Show (Cuboid a) where
  show (Cuboid x y z) = ("(Cuboid (" <> show x <> ") (" <> show y <> ") (" <> show z <> "))")

derive instance eqCuboid :: Eq a => Eq (Cuboid a)
derive instance ordCuboid :: Ord a => Ord (Cuboid a)

intersects :: forall a. Ord a => Cuboid a -> Cuboid a -> Boolean
intersects (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) = Range.intersects x1 x2 && Range.intersects y1 y2 && Range.intersects z1 z2

intersection :: forall a. Ord a => Cuboid a -> Cuboid a -> Maybe (Cuboid a)
intersection (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) = do
  x <- Range.intersection x1 x2
  y <- Range.intersection y1 y2
  z <- Range.intersection z1 z2
  Just $ Cuboid x y z

contains :: forall a. Ord a => Cuboid a -> Cuboid a -> Boolean
contains (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) = x1 `Range.contains` x2 && y1 `Range.contains` y2 && z1 `Range.contains` z2

containsPoint :: forall a. Ord a => Cuboid a -> Pos3 a -> Boolean
containsPoint (Cuboid rx ry rz) (Pos3 x y z) = Range.inRange rx x && Range.inRange ry y && Range.inRange rz z

points :: Cuboid Int -> Array (Pos3 Int)
points (Cuboid x y z) = Range.points x >>= \x -> Range.points y >>= \y -> Range.points z <#> \z -> Pos3 x y z

volume :: forall a. Ord a => Ring a => Cuboid a -> a
volume (Cuboid x y z) = Range.size x * Range.size y * Range.size z
