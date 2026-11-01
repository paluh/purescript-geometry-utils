module Geometry.Plane.FractionalCoordinates where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Geometry.Distance (SpaceUnit)
import Geometry.Plane.CoordinateSystem (class CoordinateSystem)
import Geometry.Plane.Figures.Polygons.Quadrilateral (Quadrilateral, quadrilateral)
import Geometry.Plane.Point (Point(..), point)
import Geometry.Plane.Point as Point
import Geometry.Plane.Transformations.Linear (Linear)
import Geometry.Plane.Transformations.Linear as Linear
import Geometry.Plane.Vector (Vector(..), vector)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)

data FractionalCoordinates

-- This wrapper is only to prevent orphan instance error in the below instance.
newtype FractionalPoint (c :: FractionalCoordinates) (u :: SpaceUnit) = FractionalPoint { x :: Number, y :: Number }

fractionalPoint :: forall c (u :: SpaceUnit). Number -> Number -> FractionalPoint c u
fractionalPoint x y = FractionalPoint { x, y }

newtype Basis (c :: FractionalCoordinates) (u :: SpaceUnit) = Basis
  { v1 :: Vector u
  , v2 :: Vector u
  , matrix :: Linear
  , inverse :: Linear
  }

mkBasis :: forall c u. Vector u -> Vector u -> Maybe (Basis c u)
mkBasis v1 v2 = do
  let
    matrix = do
      let
        Vector v1' = v1
        Vector v2' = v2
      -- to 3x3 matrix
      Linear.unsafe
        [ v1'.x, v2'.x
        , v1'.y, v2'.y
        ]
    inverse = case Linear.inverse matrix of
      Just m -> m
      Nothing -> unsafeCrashWith "Geometry.Plane.FractionalCoordinates.mkBasis: matrix is not invertible"
  pure $ Basis { v1, v2, inverse, matrix }

-- Also known as period parallelogram.
unitCell :: forall c u. Basis c u -> Quadrilateral u
unitCell (Basis { v1: Vector v1, v2: Vector v2 }) =
  quadrilateral
    (point 0.0 0.0)
    (point (v1.x) (v1.y))
    (point (v1.x + v2.x) (v1.y + v2.y))
    (point (v2.x) (v2.y))

baseVectors :: forall c u. Basis c u -> Vector u /\ Vector u
baseVectors (Basis { v1, v2 }) = v1 /\ v2

class FractionalCoordinatesBasis :: FractionalCoordinates -> SpaceUnit -> Constraint
class FractionalCoordinatesBasis c u | c -> u where
  getBasis :: Proxy c -> Basis c u

foreign import data EquilateralTessellationCoordinates :: SpaceUnit -> FractionalCoordinates

instance FractionalCoordinatesBasis (EquilateralTessellationCoordinates u) u where
  getBasis = do
    let
      basis = unsafePartial $ case mkBasis (vector 1.0 0.0) (vector 0.5 0.1) of
        Just b -> b
        Nothing -> unsafeCrashWith "Geometry.Plane.FractionalCoordinates.getBasis: basis is not valid"
    \_ -> basis

instance (FractionalCoordinatesBasis c u) => CoordinateSystem (FractionalPoint c u) u where
  fromCartesian p = do
    let
      Basis { inverse } = getBasis (Proxy :: Proxy c)
      Point p' = Point.linearTransform inverse p
    fractionalPoint p'.x p'.y

  toCartesian (FractionalPoint p) = do
    let
      Basis { v1: Vector v1, v2: Vector v2 } = getBasis (Proxy :: Proxy c)
    point ((p.x * v1.x) + p.y * v2.x) (p.x * v1.y + p.y * v2.y)


