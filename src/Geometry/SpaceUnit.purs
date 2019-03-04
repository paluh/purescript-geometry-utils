module Geometry.SpaceUnit where

foreign import kind SpaceUnit

newtype Distance (unit ∷ SpaceUnit) = Distance Number

