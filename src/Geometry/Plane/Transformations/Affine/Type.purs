module Geometry.Plane.Transformations.Affine.Type
  ( Affine
  , Scaling
  , array
  , fromLinear
  , inverse
  , reflection
  , rotation
  , scaling
  , toMatrix
  , translation
  , unsafeFromMatrix
  , unsafeFromArray
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Geometry.Angle (Angle)
import Geometry.Angle (cos, sin) as Angle
import Geometry.Matrix.Matrix2x2 as Matrix2x2
import Geometry.Matrix.Matrix3x3 as Matrix3x3
import Geometry.Plane.Vector as Vector
import Geometry.Plane.Vector.Types (Vector(..))
import Partial.Unsafe (unsafePartial)

-- | Tranformation 3x3 matrix with unit tag
newtype Affine :: forall k. k -> Type
newtype Affine u = Affine (Array Number)

derive instance eqAffine ∷ Eq (Affine u)

-- | Required by test unit :-(
instance showAffine ∷ Show (Affine u) where
  show (Affine m) = "(Affine.Affine u " <> show m <> ")"

-- | Affine matrices don't form a `Semiring` because
-- | addition won't work on the last row...
instance semigroupAffine ∷ Semigroup (Affine u) where
  append = multiply

instance monoidAffine ∷ Monoid (Affine u) where
  mempty = Affine [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 ]

unsafeFromArray ∷ ∀ u. Array Number → Affine u
unsafeFromArray = Affine

unsafeFromMatrix ∷ ∀ u. Matrix3x3.Matrix → Affine u
unsafeFromMatrix = Affine <<< Matrix3x3.toArray

toMatrix ∷ ∀ u. Affine u → Matrix3x3.Matrix
toMatrix (Affine m) = Matrix3x3.unsafe m

array ∷ ∀ u. Affine u → Array Number
array (Affine a) = a

multiply ∷ ∀ u. Affine u → Affine u → Affine u
multiply =
  unsafePartial
    $ case _, _ of
        Affine [ a11, a12, a13, a21, a22, a23, a31, a32, a33 ]
        , Affine [ b11, b12, b13, b21, b22, b23, _, _, _ ] →
          Affine
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

inverse :: forall u. Affine u -> Maybe (Affine u)
inverse = unsafePartial $ \(Affine [a11, a12, a13, a21, a22, a23, _, _, _]) -> do
  let
    det = a11 * a22 - a12 * a21
  if det == 0.0
  then Nothing
  else do
    let
      invDet = 1.0 / det
      inv11 = a22 * invDet
      inv12 = -a12 * invDet
      inv13 = (a12 * a23 - a13 * a22) * invDet
      inv21 = -a21 * invDet
      inv22 = a11 * invDet
      inv23 = (a13 * a21 - a11 * a23) * invDet
    Just (Affine [inv11, inv12, inv13, inv21, inv22, inv23, 0.0, 0.0, 1.0])

translation ∷ ∀ u. Vector u → Affine u
translation (Vector { x, y }) = unsafeFromArray
  [ 1.0, 0.0, x
  , 0.0, 1.0, y
  , 0.0, 0.0, 1.0
  ]

-- Should it be `Scaling or Unit?
data Scaling

scaling ∷ ∀ u. Vector Scaling → Affine u
scaling (Vector { x, y }) = unsafeFromArray
  [ x, 0.0, 0.0
  , 0.0, y, 0.0
  , 0.0, 0.0, 1.0
  ]

rotation ∷ ∀ u. Angle → Affine u
rotation a = unsafeFromArray
  [ Angle.cos a, -(Angle.sin a), 0.0
  , Angle.sin a, Angle.cos a, 0.0
  , 0.0, 0.0, 1.0
  ]

fromLinear ∷ ∀ u. Matrix2x2.Matrix → Affine u
fromLinear m = unsafePartial $ unsafeFromArray $ do
  let
    [ a11, a12
    , a21, a22
    ] = Matrix2x2.toArray m
  [ a11, a12, 0.0
  , a21, a22, 0.0
  , 0.0, 0.0, 1.0
  ]

reflection ∷ ∀ u. Vector Unit → Maybe (Affine u)
reflection v = do
  normalized <- Vector.normalize v
  let
    Vector { x, y } = normalized
    dotProd = 2.0 * Vector.dot normalized normalized
    ref11 = 1.0 - dotProd * x * x
    ref12 = -dotProd * x * y
    ref21 = -dotProd * y * x
    ref22 = 1.0 - dotProd * y * y
  pure $
    Affine
      [ ref11, ref12, 0.0
      , ref21, ref22, 0.0
      , 0.0, 0.0, 1.0
      ]
