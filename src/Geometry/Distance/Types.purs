module Geometry.Distance.Types
  ( ConversionFactor(..)
  , Distance(..)
  , SpaceUnit
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Geometry.Numbers (NonNegative) as Numbers
import Geometry.Numbers.Positive (Positive)

foreign import data SpaceUnit :: Type

-- | XXX: For every unit you should probably define your own
-- |  monomorphic constructor.
newtype Distance (unit ∷ SpaceUnit) = Distance Numbers.NonNegative
derive instance newtypeDistance ∷ Newtype (Distance u) _
derive instance eqDistance ∷ Eq (Distance u)
derive instance ordDistance ∷ Ord (Distance u)
derive instance genericDistance ∷ Generic (Distance u) _

instance semigroupDistance ∷ Semigroup (Distance u) where
  append (Distance d1) (Distance d2) = Distance (d1 + d2)

instance monoidDistance ∷ Monoid (Distance u) where
  mempty = Distance zero

newtype ConversionFactor (from ∷ SpaceUnit) (to ∷ SpaceUnit) = ConversionFactor Positive
derive instance eqConversionFactor ∷ Eq (ConversionFactor from to)
instance showConversionFactor ∷ Show (ConversionFactor from to) where
  show (ConversionFactor cf) = "(ConversionFactor " <> show cf <> ")"
