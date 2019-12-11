module Geometry.Plane.BoundingBox.Dimensions where

import Prelude

import Geometry.Distance (ConversionFactor(..), Distance(..))
import Geometry.Distance (convert) as Distance
import Geometry.Numbers.NonNegative (NonNegative(..))
import Geometry.Numbers.NonNegative (unsafe) as NonNegative
import Geometry.Numbers.Positive (Positive(..))
import Geometry.Plane.BoundingBox.AspectRatio (AspectRatio(..))
import Unsafe.Coerce (unsafeCoerce)

type Dimensions u =
  { height ∷ Distance u
  , width ∷ Distance u
  }

aspectRatio ∷ ∀ u. Dimensions u → AspectRatio
aspectRatio { height: Distance (NonNegative h), width: Distance (NonNegative w) } = AspectRatio (NonNegative.unsafe (w / h))

unsafe ∷ ∀ u. { height ∷ Number, width ∷ Number } → Dimensions u
unsafe = unsafeCoerce

convert ∷ ∀ from to. ConversionFactor from to → Dimensions from → Dimensions to
convert c@(ConversionFactor (Positive cv)) ({ height, width }) =
  { height: Distance.convert c height
  , width: Distance.convert c width
  }
