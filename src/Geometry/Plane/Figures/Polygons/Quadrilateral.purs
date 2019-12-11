module Geometry.Plane.Figures.Polygons.Quadrilateral where

import Geometry.Distance (ConversionFactor)
import Geometry.Plane.Point (Point)
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

