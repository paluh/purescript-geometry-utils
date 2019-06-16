module Geometry.Distance
  ( module Units
  , module ConversionFactor
  , Distance(..)
  , convert
  , distance
  , fromNatural
  , fromNonNegative
  , fromPositiveInt
  , fromPositiveNumber
  , toNonNegative
  , toNumber
  , ratio
  , scale
  , unsafeDistance
  , unsafeScale
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Geometry.Distance.ConversionFactor (ConversionFactor(..))
import Geometry.Distance.ConversionFactor (ConversionFactor) as ConversionFactor
import Geometry.Distance.Units (kind SpaceUnit)
import Geometry.Distance.Units (kind SpaceUnit) as Units
import Geometry.Integers (Natural, Positive) as Integers
import Geometry.Integers.Natural (toNonNegative) as Integers.Natural
import Geometry.Integers.Positive (toNatural) as Integers.Positive
import Geometry.Numbers (NonNegative, Positive) as Numbers
import Geometry.Numbers.NonNegative (NonNegative(..))
import Geometry.Numbers.NonNegative (fromNumber, unsafe) as Numbers.NonNegative
import Geometry.Numbers.Positive (toNonNegative) as Numbers.Positive
import Geometry.Numbers.Positive (toNonNegative) as Positive
import Unsafe.Coerce (unsafeCoerce)

-- | XXX: For every unit you should probably define your own
-- |  monomorphic constructor.
newtype Distance (unit ∷ SpaceUnit) = Distance Numbers.NonNegative
derive instance eqDistance ∷ Eq (Distance u)
derive instance ordDistance ∷ Ord (Distance u)
derive instance genericDistance ∷ Generic (Distance u) _

instance semigroupDistance ∷ Semigroup (Distance u) where
  append (Distance d1) (Distance d2) = Distance (d1 + d2)

instance monoidDistance ∷ Monoid (Distance u) where
  mempty = Distance zero

fromNatural ∷ ∀ u. Integers.Natural → Distance u
fromNatural n = Distance (Integers.Natural.toNonNegative n)

fromPositiveInt ∷ ∀ u. Integers.Positive → Distance u
fromPositiveInt p = Distance (Integers.Natural.toNonNegative <<< Integers.Positive.toNatural $ p)

fromPositiveNumber ∷ ∀ u. Numbers.Positive → Distance u
fromPositiveNumber p = Distance (Numbers.Positive.toNonNegative p)

fromNonNegative ∷ ∀ u. Numbers.NonNegative → Distance u
fromNonNegative n = Distance n

distance ∷ ∀ u. Number → Maybe (Distance u)
distance = map Distance <<< Numbers.NonNegative.fromNumber

toNumber ∷ ∀ u. Distance u → Number
toNumber (Distance (NonNegative n)) = n

unsafeDistance ∷ ∀ u. Number → Distance u
unsafeDistance n = unsafeCoerce n

toNonNegative ∷ ∀ u. Distance u → Numbers.NonNegative
toNonNegative (Distance n) = n

scale ∷ ∀ u. Distance u → Numbers.NonNegative → Distance u
scale (Distance d) n = Distance (d * n)

unsafeScale ∷ ∀ u. Distance u → Number → Distance u
unsafeScale (Distance d) n = Distance (d * (Numbers.NonNegative.unsafe n))

ratio ∷ ∀ u. Distance u → Distance u → NonNegative
ratio (Distance (NonNegative d1)) (Distance (NonNegative d2)) = NonNegative (d1 / d2)

convert ∷ ∀ from to. ConversionFactor from to → Distance from → Distance to
convert (ConversionFactor c) (Distance d) = Distance ((Positive.toNonNegative c) * d)

