module Geometry.Plane.Transformations.Affine where

import Geometry (Angle, toRadians)
import Geometry.Plane.Transformations.Affine.Matrix (Matrix)
import Geometry.Plane.Transformations.Translation (Translation)
import Geometry.Plane.Vector (Vector)
import Math (Radians)

foreign import toTranslationImpl ∷ ∀ u. Matrix u → Translation u

foreign import translation ∷ ∀ u. Translation u → Matrix u

foreign import scaling ∷ ∀ u. Vector → Matrix u

foreign import rotationImpl ∷ ∀ u. Radians → Matrix u

rotation ∷ ∀ u. Angle → Matrix u
rotation a = rotationImpl (toRadians a)

-- | XXX: Do we really need this strage optimization
-- foreign import translate ∷ Matrix → Vector → Matrix

