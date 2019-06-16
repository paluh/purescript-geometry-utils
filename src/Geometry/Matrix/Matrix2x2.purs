module Geometry.Matrix.Matrix2x2 where

import Prelude

import Partial.Unsafe (unsafePartial)

newtype Matrix = Matrix (Array Number)
derive instance eqMatrix ∷ Eq Matrix
instance showMatrix ∷ Show Matrix where
  show (Matrix m) = "(Matrix " <> show m <> ")"

instance semiringMatrix ∷ Semiring Matrix where
  add = unsafeAdd
  mul = unsafeMul
  zero = Matrix [ 0.0, 0.0, 0.0, 0.0 ]
  one = Matrix [ 1.0, 0.0, 0.0, 1.0 ]

instance ringMatrix ∷ Ring Matrix where
  sub = unsafeSub

unsafe ∷ Array Number → Matrix
unsafe = Matrix

array ∷ Matrix → Array Number
array (Matrix a) = a

unsafeAdd ∷ Matrix → Matrix → Matrix
unsafeAdd = unsafePartial $ case _, _ of
    Matrix [ a11, a12, a21, a22 ]
  , Matrix [ b11, b12, b21, b22 ] →
      Matrix
        [ a11 + b11, a12 + b12
        , a21 + b21, a22 + b22
        ]

unsafeSub ∷ Matrix → Matrix → Matrix
unsafeSub = unsafePartial $ case _, _ of
    Matrix [ a11, a12, a21, a22 ]
  , Matrix [ b11, b12, b21, b22 ] →
      Matrix
        [ a11 - b11, a12 - b12
        , a21 - b21, a22 - b22
        ]


unsafeMul ∷ Matrix → Matrix → Matrix
unsafeMul = unsafePartial $ case _, _ of
    Matrix [ a11, a12, a21, a22 ]
  , Matrix [ b11, b12, b21, b22 ] →
      Matrix
          [ a11 * b11 + a12 * b21
          , a11 * b12 + a12 * b22

          , a21 * b11 + a22 * b21
          , a21 * b12 + a22 * b22
          ]

