module Geometry.Plane.Transformations.Linear.Matrix where

import Prelude

foreign import data Matrix ∷ Type

instance semiringMatrix ∷ Semiring Matrix where
  add = addImpl
  zero = zeroImpl
  mul = mulImpl
  one = oneImpl

instance ringMatrix ∷ Ring Matrix where
  sub = subImpl

foreign import addImpl ∷ Matrix → Matrix → Matrix

foreign import zeroImpl ∷ Matrix

foreign import mulImpl ∷ Matrix → Matrix → Matrix

foreign import oneImpl ∷ Matrix

foreign import subImpl ∷ Matrix → Matrix → Matrix
