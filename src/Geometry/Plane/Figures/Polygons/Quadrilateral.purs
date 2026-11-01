module Geometry.Plane.Figures.Polygons.Quadrilateral where

import Prelude

import Data.Array.NonEmpty (cons') as Array.NonEmpty
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Geometry.Distance (ConversionFactor, Distance(..))
import Geometry.Numbers.NonNegative (NonNegative(..))
import Geometry.Plane.BoundingBox (BoundingBox(..))
import Geometry.Plane.BoundingBox (fromPoints) as BoundingBox
import Geometry.Plane.Figures.Polygons.Triangle (Triangle, triangle)
import Geometry.Plane.Point (Point, point)
import Geometry.Plane.Point (convert) as Point

newtype Quadrilateral u = Quadrilateral
  { p1 ∷ Point u
  , p2 ∷ Point u
  , p3 ∷ Point u
  , p4 ∷ Point u
  }

derive instance Newtype (Quadrilateral u) _
derive instance Generic (Quadrilateral u) _
derive instance Eq (Quadrilateral u)
derive instance (Ord (Point u)) => Ord (Quadrilateral u)

fromRecord :: forall r u. { p1 ∷ Point u, p2 ∷ Point u, p3 ∷ Point u, p4 ∷ Point u | r } → Quadrilateral u
fromRecord { p1, p2, p3, p4 } = Quadrilateral { p1, p2, p3, p4 }

quadrilateral ∷ ∀ u. Point u → Point u → Point u → Point u → Quadrilateral u
quadrilateral p1 p2 p3 p4 = Quadrilateral { p1, p2, p3, p4 }

convert ∷ ∀ u v. ConversionFactor u v → Quadrilateral u → Quadrilateral v
convert cf (Quadrilateral { p1, p2, p3, p4 }) = Quadrilateral
  { p1: Point.convert cf p1
  , p2: Point.convert cf p2
  , p3: Point.convert cf p3
  , p4: Point.convert cf p4
  }

boundingBox ∷ ∀ u. Quadrilateral u → BoundingBox u
boundingBox (Quadrilateral { p1, p2, p3, p4 }) = BoundingBox.fromPoints (Array.NonEmpty.cons' p1 [ p2, p3, p4 ])

fromBoundingBox ∷ ∀ u. BoundingBox u → Quadrilateral u
fromBoundingBox (BoundingBox { x, y, height: Distance (NonNegative height), width: Distance (NonNegative width) }) = Quadrilateral
  { p1: point x y
  , p2: point (x + width) y
  , p3: point (x + width) (y + height)
  , p4: point x (y + height)
  }

triangulate ∷ ∀ u. Quadrilateral u → (Triangle u) /\ (Triangle u)
triangulate (Quadrilateral { p1, p2, p3, p4 }) = do
  let
    t1 = triangle p1 p2 p3
    t2 = triangle p3 p4 p1
  t1 /\ t2
