module Geometry.Plane.Point where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Geometry.Plane.Transformation.Affine.Matrix (Matrix) as Affine
import Geometry.Plane.Transformation.Linear.Matrix (Matrix) as Linear
import Geometry.SpaceUnit (Distance(..), kind SpaceUnit)
import Math (pow, sqrt) as Math

newtype Point (unit ∷ SpaceUnit) = Point { x ∷ Number, y ∷ Number }
derive instance eqPoint ∷ Eq (Point u)
derive instance genericPoint ∷ Generic (Point u) _
instance encodeShape ∷ EncodeJson (Point u) where
  encodeJson = genericEncodeJson
instance decodeDesign ∷ DecodeJson (Point u) where
  decodeJson = genericDecodeJson

point ∷ ∀ u. Number → Number → Point u
point x y = Point { x, y }

_x ∷ ∀ u. Point u → Number
_x (Point r) = r.x

_y ∷ ∀ u. Point u → Number
_y (Point r) = r.y

_xD ∷ ∀ u. Point u → Distance u
_xD (Point r) = Distance r.x

_yD ∷ ∀ u. Point u → Distance u
_yD (Point r) = Distance r.y

distance ∷ ∀ u. Point u → Point u → Distance u
distance (Point { x: x1, y: y1 }) (Point { x: x2, y: y2 }) =
  Distance $ Math.sqrt (Math.pow (x2 - x1) 2.0 + Math.pow (y2 - y1) 2.0)

foreign import linearTransform ∷ ∀ u. Linear.Matrix → Point u → Point u

foreign import affineTransform ∷ ∀ u. Affine.Matrix u → Point u → Point u

-- translate ∷ ∀ u. Translation u → Point u → Point u
-- translate (Vector { x, y }) (Point p) =
--   Point { x: p.x + x, y: p.y + y }

