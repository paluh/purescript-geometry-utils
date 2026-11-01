module Geometry.Plane.Figures.Polygons.Triangle where

import Prelude

import Data.Array.NonEmpty (cons') as Array.NonEmpty
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Geometry.Distance (ConversionFactor, Distance(..))
import Geometry.Numbers.NonNegative (NonNegative(..))
import Geometry.Plane.BoundingBox (BoundingBox(..))
import Geometry.Plane.BoundingBox (fromPoints) as BoundingBox
import Geometry.Plane.Point (Point(..), point)
import Geometry.Plane.Point (convert) as Point

newtype Triangle u = Triangle
  { p1 ∷ Point u
  , p2 ∷ Point u
  , p3 ∷ Point u
  }

derive instance Newtype (Triangle u) _
derive instance Generic (Triangle u) _
derive instance Eq (Triangle u)
derive instance (Ord (Point u)) => Ord (Triangle u)

fromRecord :: forall r u. { p1 ∷ Point u, p2 ∷ Point u, p3 ∷ Point u | r } → Triangle u
fromRecord { p1, p2, p3 } = Triangle { p1, p2, p3 }

triangle ∷ ∀ u. Point u → Point u → Point u → Triangle u
triangle p1 p2 p3 = Triangle { p1, p2, p3 }

convert ∷ ∀ u v. ConversionFactor u v → Triangle u → Triangle v
convert cf (Triangle { p1, p2, p3 }) = Triangle
  { p1: Point.convert cf p1
  , p2: Point.convert cf p2
  , p3: Point.convert cf p3
  }

boundingBox ∷ ∀ u. Triangle u → BoundingBox u
boundingBox (Triangle { p1, p2, p3 }) = BoundingBox.fromPoints (Array.NonEmpty.cons' p1 [ p2, p3 ])

-- Collision detection functions

type CollisionResult = Boolean

isInsideTriangle :: ∀ u. Triangle u → Point u → CollisionResult
isInsideTriangle (Triangle { p1: Point p1, p2: Point p2, p3: Point p3 }) (Point pt) = do
  let
    sign p1' p2' p3' = (p1'.x - p3'.x) * (p2'.y - p3'.y) - (p2'.x - p3'.x) * (p1'.y - p3'.y)

    b1 = sign pt p1 p2 < 0.0
    b2 = sign pt p2 p3 < 0.0
    b3 = sign pt p3 p1 < 0.0

  (b1 == b2) && (b2 == b3)

trianglesCollide :: ∀ u. Triangle u → Triangle u → CollisionResult
trianglesCollide t1@(Triangle { p1, p2, p3 }) t2@(Triangle t2r) =
  isInsideTriangle t2 p1
  || isInsideTriangle t2 p2
  || isInsideTriangle t2 p3
  || isInsideTriangle t1 t2r.p1
  || isInsideTriangle t1 t2r.p2
  || isInsideTriangle t1 t2r.p3

