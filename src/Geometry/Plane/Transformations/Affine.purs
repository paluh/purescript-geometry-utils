module Geometry.Plane.Transformations.Affine
  ( module Matrix
  , rotation
  , scaling
  , translation
  )where

import Prelude

import Geometry.Angle (Angle)
import Geometry.Angle (cos, sin) as Angle
import Geometry.Plane.Transformations.Affine.Matrix (Matrix)
import Geometry.Plane.Transformations.Affine.Matrix (Matrix, unsafe) as Matrix
import Geometry.Plane.Transformations.Translation (Translation(..))
import Geometry.Plane.Vector.Types (Vector(..))

translation ∷ ∀ u. Translation u → Matrix u
translation (Translation (Vector { x, y })) = Matrix.unsafe
  [ 1.0, 0.0, x
  , 0.0, 1.0, y
  , 0.0, 0.0, 1.0
  ]

scaling ∷ ∀ u. Vector → Matrix u
scaling (Vector { x, y }) = Matrix.unsafe
  [ x, 0.0, 0.0
  , 0.0, y, 0.0
  , 0.0, 0.0, 1.0
  ]

rotation ∷ ∀ u. Angle → Matrix u
rotation a = Matrix.unsafe
  [ Angle.cos a, -(Angle.sin a), 0.0
  , Angle.sin a, Angle.cos a, 0.0
  , 0.0, 0.0, 1.0
  ]
