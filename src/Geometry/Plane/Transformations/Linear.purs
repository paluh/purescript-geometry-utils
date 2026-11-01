module Geometry.Plane.Transformations.Linear
  ( Linear
  , rotation
  , scaling
  , module Exports
  ) where

import Prelude

import Geometry.Angle (Angle)
import Geometry.Angle (cos, sin) as Angle
import Geometry.Matrix.Matrix2x2 (Matrix)
import Geometry.Matrix.Matrix2x2 as Matrix
import Geometry.Matrix.Matrix2x2 (inverse, toArray, toElements, unsafe) as Exports
import Geometry.Plane.Vector.Types (Vector(..))

type Linear = Matrix

scaling ∷ forall u. Vector u → Linear
scaling (Vector { x, y }) = Matrix.unsafe
  [ x , 0.0
  , 0.0, y
  ]

rotation ∷ Angle → Linear
rotation a = Matrix.unsafe
  [ Angle.cos a, -(Angle.sin a)
  , Angle.sin a, Angle.cos a
  ]

