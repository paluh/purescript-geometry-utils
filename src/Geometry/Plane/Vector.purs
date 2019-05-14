module Geometry.Plane.Vector
  ( module Types
  , dot
  , length
  , scale
  , vector
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Geometry.Distance (kind SpaceUnit)
import Geometry.Plane.Vector.Types (Vector(..))
import Geometry.Plane.Vector.Types (Vector(..)) as Types
import Math (sqrt) as Math

vector ∷ Number → Number → Vector
vector x y = Vector { x, y }

length ∷ Vector → Number
length (Vector { x, y }) = Math.sqrt (x * x + y * y)

scale ∷ Number → Vector → Vector
scale s (Vector { x, y }) = vector (s * x) (s * y)

dot ∷ Vector → Vector → Number
dot (Vector v1) (Vector v2) = v1.x * v2.x + v1.y * v2.y

orthogonal ∷ Vector → Vector → Boolean
orthogonal v1 v2 = (dot v1 v2) == 0.0

unit ∷ Vector → Maybe Vector
unit v@(Vector r) =
  let
    l = length v
  in
    if l == 0.0
      then Nothing
      else Just $
        Vector { x: r.x / l, y: r.y / l }

