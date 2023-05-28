module Data.Direction (Direction(..), directionX, directionY, turnLeft, turnRight, directionChar, allDirections) where

import Data.Bounded (class Ord)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.Ring (negate)

data Direction = DirUp | DirDown | DirLeft | DirRight

derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction

derive instance genericDirection :: Generic Direction _
instance showDirection :: Show Direction where
  show = genericShow

allDirections :: Array Direction
allDirections = [DirUp, DirRight, DirDown, DirLeft]

directionX ∷ Direction → Int
directionX DirRight = 1
directionX DirLeft = -1
directionX _ = 0

directionY ∷ Direction → Int
directionY DirDown = 1
directionY DirUp = -1
directionY _ = 0

turnLeft ∷ Direction → Direction
turnLeft DirUp = DirLeft
turnLeft DirLeft = DirDown
turnLeft DirDown = DirRight
turnLeft DirRight = DirUp

turnRight ∷ Direction → Direction
turnRight DirUp = DirRight
turnRight DirRight = DirDown
turnRight DirDown = DirLeft
turnRight DirLeft = DirUp

directionChar :: Direction -> Char
directionChar DirUp = '^'
directionChar DirDown = 'v'
directionChar DirLeft = '<'
directionChar DirRight = '>'

