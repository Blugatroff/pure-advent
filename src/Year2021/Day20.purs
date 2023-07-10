module Year2021.Day20 (day) where

import MeLude

import Data.Array as Array
import Data.Int (binary, fromStringAs)
import Data.Pos (Pos(..), x, y)
import Data.Set as Set
import Data.String as String
import Data.TraversableWithIndex (forWithIndex)
import Day (makeDay)
import Util (maximumOrZero, minimumOrZero, nonEmptyLines)

type Input = { enhancement :: Array Boolean, image :: Set Pos }

parse :: String -> String |? Input
parse input = do
  let lines = String.trim <$> nonEmptyLines input
  enhancement <- note "enhancement algorithm missing" $ Array.head lines
  enhancement <- parseEnhancement enhancement
  image <- parseImage $ fromMaybe [] $ Array.tail lines
  pure { enhancement, image }

parseEnhancement :: String -> String |? Array Boolean
parseEnhancement = toCharArray >>> traverse parseTile

parseImage :: Array String -> String |? Set Pos
parseImage lines = Set.fromFoldable <<< Array.concat <$> do
  forWithIndex lines \y line -> Array.catMaybes <$> do
    forWithIndex (toCharArray line) \x char -> do
      tile <- parseTile char
      pure $
        if tile then Just $ Pos x y
        else Nothing

parseTile :: Char -> String |? Boolean
parseTile '.' = Right false
parseTile '#' = Right true
parseTile c = Left $ show c <> " is not a tile"

imageDimensions :: Set Pos -> Pos /\ Pos
imageDimensions image = (Pos minX minY) /\ (Pos maxX maxY)
  where
  xs = x <$> Array.fromFoldable image
  ys = y <$> Array.fromFoldable image

  minX = minimumOrZero xs
  maxX = maximumOrZero xs
  minY = minimumOrZero ys
  maxY = maximumOrZero ys

enhance :: Boolean -> Array Boolean -> Set Pos -> Set Pos
enhance outOfBoundsState enhancement image = newImage
  where
  (Pos minX minY) /\ (Pos maxX maxY) = imageDimensions image

  neighbours (Pos x y) =
    [ Pos (x - 1) (y - 1)
    , Pos x (y - 1)
    , Pos (x + 1) (y - 1)
    , Pos (x - 1) y
    , Pos x y
    , Pos (x + 1) y
    , Pos (x - 1) (y + 1)
    , Pos x (y + 1)
    , Pos (x + 1) (y + 1)
    ]

  outOfBounds (Pos x y) = x > maxX || x < minX || y > maxY || y < minY

  bool :: forall a. a -> a -> Boolean -> a
  bool a b v = if v then a else b

  newImage = Set.fromFoldable do
    Array.range (minX - 1) (maxX + 1) >>= \x ->
      Array.range (minY - 1) (maxY + 1) # Array.mapMaybe \y ->
        let
          pos = Pos x y
          index = neighbours pos
            # map
                ( bool '1' '0' <<< \pos ->
                    if outOfBounds pos then outOfBoundsState
                    else Set.member pos image
                )
            # fromCharArray
            # fromStringAs binary
            # fromMaybe 0
          value = fromMaybe false $ Array.index enhancement index
        in
          bool (Just pos) Nothing value

solve n { enhancement, image } =
  Set.size
    $ applyN (enhance true enhancement <<< enhance false enhancement) n
    $ image

day = makeDay parse
  (Right <<< show <<< solve 1)
  (Right <<< show <<< solve 25)
