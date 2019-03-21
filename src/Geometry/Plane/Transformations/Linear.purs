module Geometry.Plane.Transformations.Linear
  ( module Matrix
  , rotation
  , scaling
  ) where


import Geometry.Angle (Angle)
import Geometry.Plane.Transformations.Linear.Matrix (Matrix) as Matrix
import Geometry.Plane.Vector (Vector)

foreign import scaling ∷ Vector → Matrix.Matrix

foreign import rotation ∷ Angle → Matrix.Matrix
