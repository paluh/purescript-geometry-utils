module Geometry.Plane.Point.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Geometry.Distance (Distance(..), kind SpaceUnit)

newtype Point (unit ∷ SpaceUnit) = Point { x ∷ Number, y ∷ Number }
derive instance eqPoint ∷ Eq (Point u)
derive instance genericPoint ∷ Generic (Point u) _
derive instance newtypePoint ∷ Newtype (Point u) _

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

