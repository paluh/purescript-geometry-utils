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
import Geometry.Matrix.Matrix2x2 (Matrix, array) as Matrix2x2
import Geometry.Numbers.Positive (Positive(..))
import Geometry.Plane.Point.Types (Point(..))
import Geometry.Plane.Point.Types (_x, _y, _xD, _yD, point, Point(..)) as Types
import Geometry.Plane.Transformations.Affine.Matrix (Matrix) as Affine
import Geometry.Plane.Transformations.Affine.Matrix (array) as Affine.Matrix
import Geometry.Plane.Transformations.Translation (Translation(..))
import Geometry.Plane.Vector.Types (Vector(..))
import Math (pow, sqrt) as Math
import Partial.Unsafe (unsafePartial)

distance ∷ ∀ u. Point u → Point u → Distance u
distance (Point { x: x1, y: y1 }) (Point { x: x2, y: y2 }) =
  unsafeDistance $ Math.sqrt (Math.pow (x2 - x1) 2.0 + Math.pow (y2 - y1) 2.0)

distance' ∷ ∀ u. Point u → Distance u
distance' (Point { x, y }) = Distance.unsafeDistance $ Math.sqrt (x * x + y * y)

linearTransform ∷ ∀ u. Matrix2x2.Matrix → Point u → Point u
linearTransform m (Point { x, y }) = unsafePartial $
  let
    [ a11, a12, a21, a22 ] = Matrix2x2.array m
  in
    Point
      { x: a11 * x + a12 * y
      , y: a21 * x + a22 * y
      }

affineTransform ∷ ∀ u. Affine.Matrix u → Point u → Point u
affineTransform m (Point { x, y }) = unsafePartial $
  let
    [ a11, a12, a13 , a21, a22, a23 , _, _, _ ] = Affine.Matrix.array m
  in
    Point
      { x: a11 * x + a12 * y + a13
      , y: a21 * x + a22 * y + a23
      }

translate ∷ ∀ u. Translation u → Point u → Point u
translate (Translation (Vector { x, y })) (Point p) =
  Point { x: p.x + x, y: p.y + y }

convert ∷ ∀ from to. ConversionFactor from to → Point from → Point to
convert (ConversionFactor (Positive c)) (Point { x, y }) =
  Point { x: c * x, y: c * y }
