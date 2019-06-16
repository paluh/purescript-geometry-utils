module Geometry.Line.Point where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Geometry.Distance (Distance(..), kind SpaceUnit)
import Geometry.Numbers.NonNegative (abs) as NonNegative

newtype Point (unit ∷ SpaceUnit) = Point Number
derive newtype instance eqPoint ∷ Eq (Point u)
derive instance genericPoint ∷ Generic (Point u) _
derive instance newtypePoint ∷ Newtype (Point u) _

distance ∷ ∀ u. Point u → Point u → Distance u
distance (Point p1) (Point p2) = Distance (NonNegative.abs (p1 - p2))

_x ∷ ∀ u. Point u → Number
_x (Point x) = x

