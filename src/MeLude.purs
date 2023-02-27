module MeLude
  ( module Data.Tuple
  , module Data.Tuple.Nested
  , module Data.Bifunctor
  , module Data.Either
  , module Data.Maybe
  , module Data.Show
  , module Data.Eq
  , module Data.Ord
  , module Data.HeytingAlgebra
  , module Data.Function
  , module Data.Semiring
  , module Data.Newtype
  , module Data.Lazy
  , module Data.List
  , module Data.Map
  , module Data.Set
  , module Data.Ring
  , module Data.Unit
  , module Data.Functor
  , module Data.Monoid
  , module Data.Unfoldable
  , module Data.Foldable
  , module Data.String.CodeUnits
  , module Data.EuclideanRing
  , module Data.Boolean
  , module Data.FunctorWithIndex
  , module Data.Semigroup
  , module Data.Traversable
  , module Data.Ordering

  , module Control.Bind
  , module Control.Applicative
  , module Control.Monad.ST

  , module Effect
  , module Effect.Class

  , type (|?)
  ) where

import Control.Bind (bind, (>>=), (=<<), discard, join, (<=<), (>=>))
import Control.Applicative (class Applicative, pure, unless, when, apply)
import Control.Monad.ST (ST)

import Data.Bifunctor (lmap)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple (Tuple, fst, snd)
import Data.Either (Either(..), note, either, hush)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Show (class Show, show)
import Data.Eq (class Eq, eq, (==), (/=), notEq)
import Data.Ord (class Ord, compare, (<), (>), (<=), (>=), lessThan, lessThanOrEq, greaterThan, greaterThanOrEq, max, min)
import Data.HeytingAlgebra (not, (&&), (||))
import Data.Function (($), (#), on, flip, identity, (>>>), (<<<), const)
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))
import Data.Ring (class Ring, negate, sub, (-))
import Data.Newtype (class Newtype, unwrap)
import Data.Lazy (Lazy, defer, force)
import Data.List (List(..), (:))
import Data.Set (Set)
import Data.Map (Map)
import Data.Unit (Unit, unit)
import Data.Functor (class Functor, flap, map, void, ($>), (<#>), (<$), (<$>), (<@>))
import Data.Monoid (class Monoid, (<>), mempty)
import Data.Unfoldable (class Unfoldable)
import Data.Foldable (class Foldable, maximum, sum, all, product, for_, intercalate, minimumBy)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.EuclideanRing (class EuclideanRing, div, mod)
import Data.Boolean (otherwise)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Semigroup (class Semigroup, append)
import Data.Traversable (class Traversable, foldl, traverse, any, minimum, sequence_, traverse_, for)
import Data.Ordering (Ordering(..))

import Effect (Effect)
import Effect.Class (liftEffect)

infix 4 type Either as |?
