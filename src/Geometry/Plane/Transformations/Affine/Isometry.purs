module Geometry.Plane.Transformations.Affine.Isometry
  ( Isometry
  , glideReflection
  , inverse
  , rotation
  , reflection
  , translation
  , unsafe
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Geometry.Angle (Angle)
import Geometry.Plane.Transformations.Affine.Type (Affine)
import Geometry.Plane.Transformations.Affine.Type as Affine
import Geometry.Plane.Vector (Vector)
import Partial.Unsafe (unsafeCrashWith)

newtype Isometry :: forall k. k -> Type
newtype Isometry u = Isometry (Affine u)

translation ∷ ∀ u. Vector u → Isometry u
translation v = Isometry $ Affine.translation v

rotation ∷ forall u. Angle → Isometry u
rotation a = Isometry $ Affine.rotation a

glideReflection ∷ forall u. Vector Unit → Vector u → Maybe (Isometry u)
glideReflection reflectionVector translationVector = do
  reflectionAffine <- Affine.reflection reflectionVector
  let
    translationAffine = Affine.translation translationVector
  pure $ Isometry $ reflectionAffine <> translationAffine

reflection ∷ forall u. Vector Unit → Maybe (Isometry u)
reflection v = Isometry <$> Affine.reflection v

inverse ∷ ∀ u. Isometry u → Isometry u
inverse (Isometry a) = Isometry $ case Affine.inverse a of
  Just a' → a'
  Nothing -> unsafeCrashWith "Isometry.inverse: inverse does not exist"

unsafe :: forall u. Affine u -> Isometry u
unsafe = Isometry
