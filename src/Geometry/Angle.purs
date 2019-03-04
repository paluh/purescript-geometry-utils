module Geometry.Angle where

import Prelude

import Data.Group (class Group)
import Math (Radians)
import Math (cos, pi, sin) as Math

data Angle
  = Radians Radians
  | Degrees Number
instance eqAngle ∷ Eq Angle where
  eq (Radians r1) (Radians r2) = r1 == r2
  eq a1 a2 = eq (toDegrees a1) (toDegrees a2)

toRadians :: Angle -> Number
toRadians (Radians r) = r
toRadians (Degrees d) = d * Math.pi / 180.0

toDegrees :: Angle -> Number
toDegrees (Radians r) = r * 180.0 / Math.pi
toDegrees (Degrees d) = d

instance semigroupAngle ∷ Semigroup Angle where
  append (Degrees d1) (Degrees d2) = Degrees (d1 + d2)
  append (Radians r1) (Radians r2) = Radians (r1 + r2)
  append angle1 angle2 = Radians (toRadians angle1) <> Radians (toRadians angle2)

instance monoidAngle ∷ Monoid Angle where
  mempty = Radians 0.0

instance groupAngle ∷ Group Angle where
  ginverse (Degrees d1) = Degrees (-d1)
  ginverse (Radians d1) = Radians (-d1)

scale ∷ Number → Angle → Angle
scale n (Radians r) = Radians (n * r)
scale n (Degrees d) = Degrees (n * d)

cos ∷ Angle → Number
cos (Radians r) = Math.cos r
cos a = Math.cos (toRadians a)

sin ∷ Angle → Number
sin (Radians r) = Math.sin r
sin a = Math.sin (toRadians a)

pi ∷ Angle
pi = Radians Math.pi

twoPi ∷ Angle
twoPi = Radians (Math.pi * 2.0)

piHalf ∷ Angle
piHalf = Radians (Math.pi / 2.0)

piFourth ∷ Angle
piFourth = Radians (Math.pi / 4.0)

