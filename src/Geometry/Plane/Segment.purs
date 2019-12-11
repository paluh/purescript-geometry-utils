module Geometry.Plane.Segment where

import Geometry.Distance.Types (ConversionFactor)
import Geometry.Plane.Point (Point)
import Geometry.Plane.Point (convert) as Point

type Segment u =
  { beginning ∷ Point u
  , end ∷ Point u
  }

convert ∷ ∀ u v. ConversionFactor u v → Segment u → Segment v
convert cf { beginning, end } =
  { beginning: Point.convert cf beginning, end: Point.convert cf end }

