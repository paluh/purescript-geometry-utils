module Geometry.Plane.Transformations.Linear
  ( rotation
  , scaling
  ) where

import Prelude

import Geometry.Angle (Angle)
import Geometry.Angle (cos, sin) as Angle
import Geometry.Matrix.Matrix2x2 (Matrix)
import Geometry.Matrix.Matrix2x2 (array, unsafe) as Matrix
import Geometry.Plane.Transformations.Affine (Matrix) as Affine
import Geometry.Plane.Transformations.Affine.Matrix (unsafe) as Affine.Matrix
import Geometry.Plane.Vector.Types (Vector(..))
import Partial.Unsafe (unsafePartial)

scaling ∷ Vector → Matrix
scaling (Vector { x, y }) = Matrix.unsafe
  [ x , 0.0
  , 0.0, y
  ]

rotation ∷ Angle → Matrix
rotation a = Matrix.unsafe
  [ Angle.cos a, -(Angle.sin a)
  , Angle.sin a, Angle.cos a
  ]

affine ∷ ∀ u. Matrix → Affine.Matrix u
affine m = unsafePartial $ Affine.Matrix.unsafe $
  let
    [ a11, a12
    , a21, a22
    ] = Matrix.array m
  in
    [ a11, a12, 0.0
    , a21, a22, 0.0
    , 0.0, 0.0, 1.0
    ]
