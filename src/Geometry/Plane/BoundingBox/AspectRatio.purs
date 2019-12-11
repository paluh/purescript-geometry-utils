module Geometry.Plane.BoundingBox.AspectRatio where

import Prelude

import Geometry.Numbers.NonNegative (NonNegative)
import Geometry.Numbers.NonNegative (toNumber) as NonNegative

-- | We should probably provide restrict here to `Positive`
newtype AspectRatio = AspectRatio NonNegative
derive instance eqAspectRatio ∷ Eq AspectRatio
derive instance ordAspectRatio ∷ Ord AspectRatio

-- | I'm not sure if this has a lot of sens - something 
-- | like stretching / compressing
-- instance semigroupAspectRatio ∷ Semigroup AspectRatio where
--   append (AspectRatio a1) (AspectRatio a2) = AspectRatio (a1 * a2)
-- 
-- instance monoidAspectRatio ∷ Monoid AspectRatio where
--   mempty = AspectRatio one

toNonNegative ∷ AspectRatio → NonNegative
toNonNegative (AspectRatio n) = n

toNumber ∷ AspectRatio → Number
toNumber = toNonNegative >>> NonNegative.toNumber
