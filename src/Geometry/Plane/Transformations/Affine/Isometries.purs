module Geometry.Plane.Transformations.Affine.Isometries
  ( Isometry
  , translation
  )
  where

import Prelude

import Geometry.Distance (kind SpaceUnit)
import Geometry.Plane.Transformations.Affine.Matrix (Matrix)
import Geometry.Plane.Transformations.Affine.Matrix (unsafe) as Matrix
import Geometry.Plane.Vector (Vector(..))

newtype Isometry u = Isometry (Matrix u)

translation ∷ ∀ u. Vector → Isometry u
translation (Vector { x, y }) = Isometry $ Matrix.unsafe
  [ 1.0, 0.0, x
  , 0.0, 1.0, y
  , 0.0, 0.0, 1.0
  ]

newtype Rigid u = Rigid (Matrix u)


