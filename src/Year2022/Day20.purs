module Year2022.Day20 (day) where

import MeLude

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Array.ST.Extra (findIndex)
import Day (makeDay)
import JS.BigInt (BigInt, fromInt, toInt)
import Util (parseInt, nonEmptyLines)

parse ∷ String → String |? (Array BigInt)
parse = nonEmptyLines >>> traverse (parseInt >>> map fromInt)

mix :: forall r. STArray r (Int /\ BigInt) -> Int /\ BigInt -> ST r Unit
mix slots slot@(_ /\ value) = do
  size <- fromInt <$> STArray.length slots
  index <- findIndex slots (eq slot)
  case index of
    Nothing -> pure unit
    Just index -> do
      _ <- STArray.splice index 1 [] slots
      let newIndex = (fromInt index + value) `mod` (size - one)
      let newInBounds = fromMaybe 0 $ toInt $ if newIndex < zero then newIndex + size - one else newIndex
      _ <- STArray.splice (newInBounds) 0 [ slot ] slots
      pure unit

groveCoords :: Array (Int /\ BigInt) -> Array BigInt
groveCoords slots = fromMaybe [] do
  indexOfTheZero <- Array.findIndex (snd >>> eq zero) slots
  let locations = [ 1000, 2000, 3000 ]
  let size = Array.length slots
  let indexes = locations <#> add indexOfTheZero >>> (_ `mod` size)
  map snd <$> traverse (Array.index slots) indexes

solvePartOne :: Array BigInt -> BigInt
solvePartOne numbers = ST.run do
  let slots = mapWithIndex (/\) numbers
  slotsST <- STArray.thaw slots
  ST.foreach slots $ mix slotsST
  sum <<< groveCoords <$> STArray.freeze slotsST

decryptionKey :: BigInt
decryptionKey = fromInt 811589153

solvePartTwo :: Array BigInt -> BigInt
solvePartTwo = map (mul decryptionKey) >>> \numbers -> ST.run do
  let slots = mapWithIndex (/\) numbers
  slotsST <- STArray.thaw slots
  ST.for 0 10 \_ ->
    ST.foreach slots $ mix slotsST
  sum <<< groveCoords <$> (STArray.freeze slotsST)

day = makeDay parse
  (Right <<< show <<< solvePartOne)
  (Right <<< show <<< solvePartTwo)
