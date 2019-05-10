module Geometry.Distance
  ( module Units
  , ConversionFactor(..)
  , Distance(..)
  , convert
  , inverse
  , ratio
  , scale
  , unsafeScale
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Geometry.Distance.Units (kind SpaceUnit)
import Geometry.Distance.Units (kind SpaceUnit) as Units
import Geometry.Numbers (NonNegative(..))

-- | XXX: For every unit you should probably define your own
-- |  monomorphic constructor.
newtype Distance (unit ∷ SpaceUnit) = Distance Number
derive instance eqDistance ∷ Eq (Distance u)
derive instance ordDistance ∷ Ord (Distance u)
derive instance genericDistance ∷ Generic (Distance u) _

instance semigroupDistance ∷ Semigroup (Distance u) where
  append (Distance d1) (Distance d2) = Distance (d1 + d2)

instance monoidDistance ∷ Monoid (Distance u) where
  mempty = Distance 0.0

scale ∷ ∀ u. Distance u → NonNegative → Distance u
scale (Distance d) (NonNegative n) = Distance (d * n)

unsafeScale ∷ ∀ u. Distance u → Number → Distance u
unsafeScale (Distance d) n = Distance (d * n)

ratio ∷ ∀ u. Distance u → Distance u → Number
ratio (Distance d1) (Distance d2) = d1 / d2

newtype ConversionFactor (from ∷ SpaceUnit) (to ∷ SpaceUnit) = ConversionFactor Number

convert ∷ ∀ from to. ConversionFactor from to → Distance from → Distance to
convert (ConversionFactor c) (Distance d) = Distance (c * d)

inverse ∷ ∀ from to. ConversionFactor from to → ConversionFactor to from
inverse (ConversionFactor c) = ConversionFactor (1.0 / c)
