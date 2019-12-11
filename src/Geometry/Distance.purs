module Geometry.Distance
  ( module Types
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
  , unsafe
  , unsafeScale
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Geometry.Distance.Types (ConversionFactor(..), Distance(..))
import Geometry.Distance.Types (ConversionFactor(..), Distance(..), kind SpaceUnit) as Types
import Geometry.Integers (Natural, Positive) as Integers
import Geometry.Integers.Natural (toNonNegative) as Integers.Natural
import Geometry.Integers.Positive (toNatural) as Integers.Positive
import Geometry.Numbers (NonNegative, Positive) as Numbers
import Geometry.Numbers.NonNegative (NonNegative(..))
import Geometry.Numbers.NonNegative (fromNumber, unsafe) as Numbers.NonNegative
import Geometry.Numbers.Positive (toNonNegative) as Numbers.Positive
import Geometry.Numbers.Positive (toNonNegative) as Positive
import Unsafe.Coerce (unsafeCoerce)

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

unsafe ∷ ∀ u. Number → Distance u
unsafe n = unsafeCoerce n

toNonNegative ∷ ∀ u. Distance u → Numbers.NonNegative
toNonNegative (Distance n) = n

scale ∷ ∀ u. Distance u → Numbers.NonNegative → Distance u
scale (Distance d) n = Distance (d * n)

unsafeScale ∷ ∀ u. Number → Distance u → Distance u
unsafeScale n (Distance d) = Distance (d * (Numbers.NonNegative.unsafe n))

ratio ∷ ∀ u. Distance u → Distance u → NonNegative
ratio (Distance (NonNegative d1)) (Distance (NonNegative d2)) = NonNegative (d1 / d2)

convert ∷ ∀ from to. ConversionFactor from to → Distance from → Distance to
convert (ConversionFactor c) (Distance d) = Distance ((Positive.toNonNegative c) * d)

