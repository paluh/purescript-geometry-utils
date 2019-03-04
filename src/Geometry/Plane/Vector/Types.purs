module Geometry.Plane.Vector.Types where

import Prelude

import Data.Group (class Group)
import Geometry.SpaceUnit (kind SpaceUnit)

newtype Vector = Vector { x ∷ Number, y ∷ Number }
derive instance eqVector :: Eq Vector

instance semigroupVector ∷ Semigroup Vector where
  append (Vector v1) (Vector v2) = Vector { x: v1.x + v2.x, y: v1.y + v2.y }

instance monoidVector ∷ Monoid Vector where
  mempty = Vector { x: 0.0, y: 0.0 }

instance groupVector ∷ Group Vector where
  ginverse (Vector { x, y }) = Vector { x: -x, y: -y }

newtype Translation (u ∷ SpaceUnit) = Translation Vector
derive instance eqTranslation :: Eq (Translation u)
