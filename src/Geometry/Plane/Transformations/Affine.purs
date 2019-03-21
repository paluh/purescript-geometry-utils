module Geometry.Plane.Transformations.Affine
  ( module Matrix
  , rotation
  , scaling
  , toTranslation
  , translation
  )where

import Geometry.Angle (Angle)
import Geometry.Plane.Transformations.Affine.Matrix (Matrix)
import Geometry.Plane.Transformations.Affine.Matrix (Matrix) as Matrix
import Geometry.Plane.Transformations.Translation (Translation)
import Geometry.Plane.Vector (Vector)

foreign import toTranslation ∷ ∀ u. Matrix u → Translation u

foreign import translation ∷ ∀ u. Translation u → Matrix u

foreign import scaling ∷ ∀ u. Vector → Matrix u

foreign import rotation ∷ ∀ u. Angle → Matrix u

