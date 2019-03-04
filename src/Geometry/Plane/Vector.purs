module Geometry.Plane.Vector
  ( module Types
  , fromPoints
  , initialInOrigin
  , length
  , scale
  , vector
  )
  where

import Prelude

import Geometry.Plane.Point (Point(..))
import Geometry.Plane.Vector.Types (Vector(..), Translation(..))
import Geometry.Plane.Vector.Types (Vector(..), Translation(..)) as Types
import Geometry.SpaceUnit (kind SpaceUnit)
import Math (sqrt) as Math

vector ∷ Number → Number → Vector
vector x y = Vector { x, y }

fromPoints ∷ ∀ u. Point u → Point u → Translation u
fromPoints (Point initial) (Point terminal) = Translation $ Vector $
  { x: terminal.x - initial.x, y: terminal.y - initial.y }

initialInOrigin ∷ ∀ u. Point u → Translation u
initialInOrigin (Point terminal) = Translation (Vector terminal)

length ∷ Vector → Number
length (Vector { x, y }) = Math.sqrt (x * x + y * y)

scale ∷ Number → Vector → Vector
scale s (Vector { x, y }) = vector (s * x) (s * y)

