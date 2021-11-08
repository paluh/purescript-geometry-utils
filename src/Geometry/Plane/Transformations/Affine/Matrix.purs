module Geometry.Plane.Transformations.Affine.Matrix
  ( Matrix
  , array
  , matrix3x3
  , unsafe
  ) where

import Prelude
import Geometry.Distance (kind SpaceUnit)
import Geometry.Matrix.Matrix3x3 (Matrix, unsafe) as Matrix3x3
import Partial.Unsafe (unsafePartial)

-- | Tranformation 3x3 matrix with unit tag
newtype Matrix (u ∷ SpaceUnit)
  = Matrix (Array Number)

derive instance eqMatrix ∷ Eq (Matrix u)

-- | Required by test unit :-(
instance showMatrix ∷ Show (Matrix u) where
  show (Matrix m) = "(Affine.Matrix u " <> show m <> ")"

-- | Affine matrices don't form a `Semiring` because
-- | addition won't work on the last row...
instance semigroupMatrix ∷ Semigroup (Matrix u) where
  append = multiply

instance monoidMatrix ∷ Monoid (Matrix u) where
  mempty = Matrix [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 ]

unsafe ∷ ∀ u. Array Number → Matrix u
unsafe = Matrix

array ∷ ∀ u. Matrix u → Array Number
array (Matrix a) = a

matrix3x3 ∷ ∀ u. Matrix u → Matrix3x3.Matrix
matrix3x3 (Matrix m) = Matrix3x3.unsafe m

multiply ∷ ∀ u. Matrix u → Matrix u → Matrix u
multiply =
  unsafePartial
    $ case _, _ of
        Matrix [ a11, a12, a13, a21, a22, a23, a31, a32, a33 ]
        , Matrix [ b11, b12, b13, b21, b22, b23, _, _, b33 ] →
          Matrix
            [ a11 * b11 + a12 * b21 -- + a13 * b31
            , a11 * b12 + a12 * b22 -- + a13 * b32
            , a11 * b13 + a12 * b23 + a13 -- * b33
            , a21 * b11 + a22 * b21 -- + a23 * b31
            , a21 * b12 + a22 * b22 -- + a23 * b32
            , a21 * b13 + a22 * b23 + a23 -- * b33
            -- 0.0, 0.0, 1.0
            , a31
            , a32
            , a33
            ]
