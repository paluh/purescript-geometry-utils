module Geometry.Plane.Transformations.Linear
  ( module Matrix
  ) where


import Geometry.Angle (Angle, toRadians)
import Geometry.Plane.Transformations.Linear.Matrix (Matrix) as Matrix
import Geometry.Plane.Vector (Vector)
import Math (Radians)

foreign import scaling ∷ Vector → Matrix.Matrix

foreign import rotationImpl ∷ Radians → Matrix.Matrix

rotation ∷ Angle → Matrix.Matrix
rotation a = rotationImpl (toRadians a)
