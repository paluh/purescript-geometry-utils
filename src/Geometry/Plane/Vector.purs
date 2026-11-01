module Geometry.Plane.Vector
  ( module Types
  , colinear
  , convert
  , dot
  , fromPoints
  , lengthDistance
  , lengthValue
  , normalize
  , orthogonal
  , position
  , position'
  , scale
  , terminal
  , vector
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Geometry.Distance (ConversionFactor(..), Distance)
import Geometry.Distance as Distance
import Geometry.Numbers.NonNegative (NonNegative(..))
import Geometry.Numbers.Positive (Positive(..))
import Geometry.Plane.Point.Types (Point(..))
import Geometry.Plane.Vector.Types (Vector(..))
import Geometry.Plane.Vector.Types (Vector(..), _x, _y) as Types

vector ∷ forall u. Number → Number → Vector u
vector x y = Vector { x, y }

lengthDistance ∷ forall u. Vector u → Distance u
lengthDistance (Vector { x, y }) = Distance.unsafe $ sqrt (x * x + y * y)

lengthValue ∷ forall u. Vector u → NonNegative
lengthValue (Vector { x, y }) = NonNegative $ sqrt (x * x + y * y)

scale ∷ forall u. Number → Vector u → Vector u
scale s (Vector { x, y }) = vector (s * x) (s * y)

dot ∷ forall u. Vector u → Vector u → Number
dot (Vector v1) (Vector v2) = v1.x * v2.x + v1.y * v2.y

orthogonal ∷ forall u. Vector u → Vector u → Boolean
orthogonal v1 v2 = (dot v1 v2) == 0.0

normalize ∷ forall u. Vector u → Maybe (Vector Unit)
normalize v@(Vector r) = do
  let
    NonNegative l = lengthValue v
  if l == 0.0
    then Nothing
    else Just $
      Vector { x: r.x / l, y: r.y / l }

crossProduct ∷ forall u. Vector u → Vector u → Number
crossProduct (Vector v1) (Vector v2) = v1.x * v2.y - v1.y * v2.x

colinear :: forall u. Vector u -> Vector u -> Boolean
colinear (Vector v1) (Vector v2) = crossProduct (Vector v1) (Vector v2) == 0.0

convert ∷ ∀ from to. ConversionFactor from to → Vector from → Vector to
convert (ConversionFactor (Positive c)) (Vector { x, y }) =
  Vector { x: c * x, y: c * y }

position ∷ ∀ u. Point u → Vector u
position (Point t) = Vector t

position' ∷ ∀ u. Number → Number → Vector u
position' x y = Vector { x, y }

fromPoints ∷ ∀ u. Point u → Point u → Vector u
fromPoints (Point init) (Point term) = Vector { x: term.x - init.x, y: term.y - init.y }

terminal ∷ ∀ u. Vector u → Point u
terminal (Vector t) = Point t
