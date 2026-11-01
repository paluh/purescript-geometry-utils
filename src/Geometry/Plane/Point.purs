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

import Data.Number (pow, sqrt)
import Geometry.Distance (ConversionFactor(..), Distance)
import Geometry.Distance (unsafe) as Distance
import Geometry.Numbers.Positive (Positive(..))
import Geometry.Plane.Point.Types (Point(..))
import Geometry.Plane.Point.Types (_x, _y, _xD, _yD, point, Point(..)) as Types
import Geometry.Plane.Transformations.Affine.Type (Affine)
import Geometry.Plane.Transformations.Affine.Type as Affine
import Geometry.Plane.Transformations.Linear (Linear)
import Geometry.Plane.Transformations.Linear as Linear
import Geometry.Plane.Vector.Types (Vector(..))
import Partial.Unsafe (unsafePartial)

distance ∷ ∀ u. Point u → Point u → Distance u
distance (Point { x: x1, y: y1 }) (Point { x: x2, y: y2 }) =
  Distance.unsafe $ sqrt (pow (x2 - x1) 2.0 + pow (y2 - y1) 2.0)

distance' ∷ ∀ u. Point u → Distance u
distance' (Point { x, y }) = Distance.unsafe $ sqrt (x * x + y * y)

linearTransform ∷ ∀ u. Linear → Point u → Point u
linearTransform m (Point { x, y }) = unsafePartial $
  let
    [ a11, a12, a21, a22 ] = Linear.toArray m
  in
    Point
      { x: a11 * x + a12 * y
      , y: a21 * x + a22 * y
      }

affineTransform ∷ ∀ u. Affine u → Point u → Point u
affineTransform m (Point { x, y }) = unsafePartial $
  let
    [ a11, a12, a13 , a21, a22, a23 , _, _, _ ] = Affine.array m
  in
    Point
      { x: a11 * x + a12 * y + a13
      , y: a21 * x + a22 * y + a23
      }

translate ∷ ∀ u. Vector u → Point u → Point u
translate (Vector { x, y }) (Point p) =
  Point { x: p.x + x, y: p.y + y }

convert ∷ ∀ from to. ConversionFactor from to → Point from → Point to
convert (ConversionFactor (Positive c)) (Point { x, y }) =
  Point { x: c * x, y: c * y }
