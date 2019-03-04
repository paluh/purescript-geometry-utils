module Geometry.Plane.Transformation.Affine.Matrix where

import Prelude

import Geometry.SpaceUnit (kind SpaceUnit)

foreign import data Matrix ∷ SpaceUnit → Type

instance semiringMatrix ∷ Semiring (Matrix u) where
  add = addImpl
  zero = zeroImpl
  mul = mulImpl
  one = oneImpl

instance ringMatrix ∷ Ring (Matrix u) where
  sub = subImpl

foreign import addImpl ∷ ∀ u. Matrix u → Matrix u → Matrix u

foreign import zeroImpl ∷ ∀ u. Matrix u

foreign import mulImpl ∷ ∀ u. Matrix u → Matrix u → Matrix u

foreign import oneImpl ∷ ∀ u. Matrix u

foreign import subImpl ∷ ∀ u. Matrix u → Matrix u → Matrix u

