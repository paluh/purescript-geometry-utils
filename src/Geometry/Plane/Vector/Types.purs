module Geometry.Plane.Vector.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Group (class Group)
import Data.Newtype (class Newtype)

newtype Vector :: forall k. k -> Type
newtype Vector u = Vector { x ∷ Number, y ∷ Number }

derive instance Eq (Vector u)
derive instance Generic (Vector u) _
derive instance Newtype (Vector u) _

instance Semigroup (Vector u) where
  append (Vector v1) (Vector v2) = do
    let
      (v :: Vector u) = Vector { x: v1.x + v2.x, y: v1.y + v2.y }
    v

instance Monoid (Vector u) where
  mempty = Vector { x: 0.0, y: 0.0 }

instance Group (Vector u) where
  ginverse (Vector { x, y }) = Vector { x: -x, y: -y }

_x ∷ forall u. Vector u → Number
_x (Vector r) = r.x

_y ∷ forall u. Vector u → Number
_y (Vector r) = r.y

