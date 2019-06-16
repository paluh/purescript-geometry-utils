module Geometry.Matrix.Matrix3x3
  ( Matrix
  , array
  , unsafe
  )
  where

import Prelude

import Partial.Unsafe (unsafePartial)

newtype Matrix = Matrix (Array Number)
derive instance eqMatrix ∷ Eq Matrix
instance showMatrix ∷ Show Matrix where
  show (Matrix m) = "(Matrix " <> show m <> ")"

instance semiringMatrix ∷ Semiring Matrix where
  add = unsafeAdd
  mul = unsafeMul
  zero = Matrix [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ]
  one = Matrix [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 ]

instance ringMatrix ∷ Ring Matrix where
  sub = unsafeSub

array ∷ Matrix → Array Number
array (Matrix a) = a

unsafe ∷ Array Number → Matrix
unsafe = Matrix

unsafeAdd ∷ Matrix → Matrix → Matrix
unsafeAdd = unsafePartial $ case _, _ of
    Matrix [ a11, a12, a13, a21, a22, a23, a31, a32, a33 ]
  , Matrix [ b11, b12, b13 , b21, b22, b23 , b31, b32, b33 ] →
      Matrix
        [ a11 + b11, a12 + b12, a13 + b13
        , a21 + b21, a22 + b22, a23 + b23
        , a31 + b31, a32 + b32, a33 + b33
        ]

unsafeMul ∷ Matrix → Matrix → Matrix
unsafeMul = unsafePartial $ case _, _ of
    Matrix [ a11, a12, a13, a21, a22, a23, a31, a32, a33 ]
  , Matrix [ b11, b12, b13 , b21, b22, b23 , b31, b32, b33 ] →
      Matrix
          [ a11 * b11 + a12 * b21 + a13 * b31
          , a11 * b12 + a12 * b22 + a13 * b32
          , a11 * b13 + a12 * b23 + a13 * b33

          , a21 * b11 + a22 * b21 + a23 * b31
          , a21 * b12 + a22 * b22 + a23 * b32
          , a21 * b13 + a22 * b23 + a23 * b33

          , a31 * b11 + a32 * b21 + a33 * b31
          , a31 * b12 + a32 * b22 + a33 * b32
          , a31 * b13 + a32 * b23 + a33 * b33
          ]

unsafeSub ∷ Matrix → Matrix → Matrix
unsafeSub = unsafePartial $ case _, _ of
    Matrix [ a11, a12, a13, a21, a22, a23, a31, a32, a33 ]
  , Matrix [ b11, b12, b13 , b21, b22, b23 , b31, b32, b33 ] →
      Matrix
        [ a11 - b11, a12 - b12, a13 - b13
        , a21 - b21, a22 - b22, a23 - b23
        , a31 - b31, a32 - b32, a33 - b33
        ]

