module Geometry.Plane.Figures.Polygons.Quadrilateral where

import Prelude

import Data.Array.NonEmpty (cons') as Array.NonEmpty
import Geometry.Distance (ConversionFactor, Distance(..))
import Geometry.Numbers.NonNegative (NonNegative(..))
import Geometry.Plane.BoundingBox (BoundingBox(..))
import Geometry.Plane.BoundingBox (fromPoints) as BoundingBox
import Geometry.Plane.Point (Point, point)
import Geometry.Plane.Point (convert) as Point

type Quadrilateral u =
  { first ∷ Point u
  , second ∷ Point u
  , third ∷ Point u
  , fourth ∷ Point u
  }

convert ∷ ∀ u v. ConversionFactor u v → Quadrilateral u → Quadrilateral v
convert cf { first, second, third, fourth } =
  { first: Point.convert cf first
  , second: Point.convert cf second
  , third: Point.convert cf third
  , fourth: Point.convert cf fourth
  }

boundingBox ∷ ∀ u. Quadrilateral u → BoundingBox u
boundingBox { first, second, third, fourth } = BoundingBox.fromPoints (Array.NonEmpty.cons' first [ second, third, fourth ])

fromBoundingBox ∷ ∀ u. BoundingBox u → Quadrilateral u
fromBoundingBox (BoundingBox { x, y, height: Distance (NonNegative height), width: Distance (NonNegative width) }) =
  { first: point x y
  , second: point (x + width) y
  , third: point (x + width) (y + width)
  , fourth: point x (y + width)
  }

