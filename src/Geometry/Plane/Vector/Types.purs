module Geometry.Plane.Vector.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Group (class Group)
import Data.Newtype (class Newtype)
import Geometry.Distance (kind SpaceUnit)

newtype Vector = Vector { x ∷ Number, y ∷ Number }
derive instance eqVector ∷ Eq Vector
derive instance genericVector ∷ Generic Vector _
derive instance newtypeVector ∷ Newtype Vector _

instance semigroupVector ∷ Semigroup Vector where
  append (Vector v1) (Vector v2) = Vector { x: v1.x + v2.x, y: v1.y + v2.y }

instance monoidVector ∷ Monoid Vector where
  mempty = Vector { x: 0.0, y: 0.0 }

instance groupVector ∷ Group Vector where
  ginverse (Vector { x, y }) = Vector { x: -x, y: -y }

_x ∷ Vector → Number
_x (Vector r) = r.x

_y ∷ Vector → Number
_y (Vector r) = r.y

