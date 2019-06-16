module Geometry.Plane.Point
  ( module Types
  , convert
  , distance
  , distance'
  , linearTransform
  , affineTransform
  , translate
  ) where

import Prelude

import Geometry.Distance (Distance, unsafeDistance)
import Geometry.Distance (unsafeDistance) as Distance
import Geometry.Distance.ConversionFactor (ConversionFactor(..))
import Geometry.Numbers.Positive (Positive(..))
import Geometry.Plane.Point.Types (Point(..))
import Geometry.Plane.Point.Types (_x, _y, _xD, _yD, point, Point(..)) as Types
import Geometry.Plane.Transformations.Affine.Matrix (Matrix) as Affine
import Geometry.Plane.Transformations.Linear.Matrix (Matrix) as Linear
import Geometry.Plane.Transformations.Translation (Translation(..))
import Geometry.Plane.Vector.Types (Vector(..))
import Math (pow, sqrt) as Math

distance ∷ ∀ u. Point u → Point u → Distance u
distance (Point { x: x1, y: y1 }) (Point { x: x2, y: y2 }) =
  unsafeDistance $ Math.sqrt (Math.pow (x2 - x1) 2.0 + Math.pow (y2 - y1) 2.0)

distance' ∷ ∀ u. Point u → Distance u
distance' (Point { x, y }) = Distance.unsafeDistance $ Math.sqrt (x * x + y * y)

foreign import linearTransform ∷ ∀ u. Linear.Matrix → Point u → Point u

foreign import affineTransform ∷ ∀ u. Affine.Matrix u → Point u → Point u

translate ∷ ∀ u. Translation u → Point u → Point u
translate (Translation (Vector { x, y })) (Point p) =
  Point { x: p.x + x, y: p.y + y }

convert ∷ ∀ from to. ConversionFactor from to → Point from → Point to
convert (ConversionFactor (Positive c)) (Point { x, y }) =
  Point { x: c * x, y: c * y }
