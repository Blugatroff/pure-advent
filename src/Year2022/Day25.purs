module Year2022.Day25 (day) where

import MeLude

import Day (makeDay)
import JS.BigInt (BigInt, fromInt, toInt)
import Util (nonEmptyLines)

data SnafuDigit = Two | One | Zero | Minus | DoubleMinus

instance showSnafuDigit :: Show SnafuDigit where
  show = case _ of
    Two -> "2"
    One -> "1"
    Zero -> "0"
    Minus -> "-"
    DoubleMinus -> "="

parse :: String -> String |? Array (Array SnafuDigit)
parse input = do
  for (nonEmptyLines input) \line -> do
    for (toCharArray line) case _ of
      '2' -> Right Two
      '1' -> Right One
      '0' -> Right Zero
      '-' -> Right Minus
      '=' -> Right DoubleMinus
      cha -> Left $ "invalid digit: " <> show cha

snafuDigitToInt :: SnafuDigit -> Int
snafuDigitToInt = case _ of
  Two -> 2
  One -> 1
  Zero -> 0
  Minus -> -1
  DoubleMinus -> -2

snafuToInt :: Array SnafuDigit -> BigInt
snafuToInt digits = foldl fold zero digits
  where
  fold n digit = n * five + fromInt (snafuDigitToInt digit)

two = one + one
five = two + two + one

intToSnafu :: BigInt -> Array SnafuDigit
intToSnafu n | n <= two && n >= -two = case toInt n of
  Just 2 -> [ Two ]
  Just 1 -> [ One ]
  Just 0 -> [ Zero ]
  Just (-1) -> [ Minus ]
  Just (-2) -> [ DoubleMinus ]
  _ -> []
intToSnafu n = do
  let left = (n + two) `div` five
  let right = n - left * five
  intToSnafu left <> intToSnafu right

showSnafu :: forall f. Foldable f => f SnafuDigit -> String
showSnafu = foldMap show

solvePartOne :: Array (Array SnafuDigit) -> String
solvePartOne snafus = showSnafu (intToSnafu (sum (map snafuToInt snafus)))

day = makeDay parse
  (Right <<< solvePartOne)
  (const $ Right "This puzzle doesn't have part two")
