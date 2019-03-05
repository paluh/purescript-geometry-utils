module Geometry.Plane.Vector
  ( module Types
  , length
  , scale
  , vector
  )
  where

import Prelude

import Geometry.Plane.Vector.Types (Vector(..)) as Types
import Geometry.Plane.Vector.Types (Vector(..))
import Geometry.SpaceUnit (kind SpaceUnit)
import Math (sqrt) as Math

vector ∷ Number → Number → Vector
vector x y = Vector { x, y }

length ∷ Vector → Number
length (Vector { x, y }) = Math.sqrt (x * x + y * y)

scale ∷ Number → Vector → Vector
scale s (Vector { x, y }) = vector (s * x) (s * y)

