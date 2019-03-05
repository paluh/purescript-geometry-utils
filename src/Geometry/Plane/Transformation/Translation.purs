module Geometry.Plane.Translation where

import Prelude

import Data.Group (class Group)
import Geometry.Plane.Point (Point(..))
import Geometry.Plane.Vector (Vector(..))
import Geometry.SpaceUnit (Distance(..), kind SpaceUnit)
import Math (sqrt) as Math

newtype Translation (u ∷ SpaceUnit) = Translation Vector
derive instance eqTranslation :: Eq (Translation u)
derive newtype instance semigroupTranslation ∷ Semigroup (Translation u)
derive newtype instance monoidTranslation ∷ Monoid (Translation u)
derive newtype instance groupTranslation ∷ Group (Translation u)

translation ∷ ∀ u. Distance u → Distance u → Translation u
translation (Distance x) (Distance y) = Translation $ Vector { x,  y }

fromPoints ∷ ∀ u. Point u → Point u → Translation u
fromPoints (Point initial) (Point terminal) = Translation $ Vector $
  { x: terminal.x - initial.x, y: terminal.y - initial.y }

initialInOrigin ∷ ∀ u. Point u → Translation u
initialInOrigin (Point terminal) = Translation (Vector terminal)

length ∷ ∀ u. Translation u → Distance u
length (Translation (Vector { x, y })) = Distance (Math.sqrt (x * x + y * y))

scale ∷ ∀ u. Number → Translation u → Translation u
scale s (Translation (Vector { x, y })) = translation (Distance (s * x)) (Distance (s * y))

