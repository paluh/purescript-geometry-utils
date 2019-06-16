module Geometry.Angle
  ( Angle(..)
  , cos
  , fromDegrees
  , pi
  , scale
  , sin
  , toDegrees
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Group (class Group)
import Math (Radians)
import Math (cos, pi, sin) as Math

newtype Angle = Angle Radians

instance eqAngle ∷ Eq Angle where
  eq (Angle r1) (Angle r2) = r1 == r2
derive instance genericAngle ∷ Generic Angle _

instance semigroupAngle ∷ Semigroup Angle where
  append (Angle r1) (Angle r2) = Angle (r1 + r2)

instance monoidAngle ∷ Monoid Angle where
  mempty = Angle 0.0

instance groupAngle ∷ Group Angle where
  ginverse (Angle r) = Angle (-r)

fromDegrees ∷ Number -> Angle
fromDegrees d = Angle ((d * Math.pi) / 180.0)

toDegrees ∷ Angle → Number
toDegrees (Angle a) = (a * 180.0) / Math.pi

scale ∷ Number → Angle → Angle
scale n (Angle r) = Angle (n * r)

cos ∷ Angle → Number
cos (Angle r) = Math.cos r

sin ∷ Angle → Number
sin (Angle r) = Math.sin r

pi ∷ Angle
pi = Angle Math.pi
