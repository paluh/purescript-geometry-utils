module Geometry.Distance.ConversionFactor
  ( ConversionFactor(..)
  , compose
  , identity
  , inverse
  )
  where

import Prelude

import Data.Typelevel.Num (D1)
import Geometry.Distance.Units (kind SpaceUnit)
import Geometry.Integers.Positive (reflectPos, toPositiveNumber) as Integers.Positive
import Geometry.Numbers.Positive (Positive(..))
import Type.Prelude (Proxy(..))

newtype ConversionFactor (from ∷ SpaceUnit) (to ∷ SpaceUnit) = ConversionFactor Positive

-- We are not able to provide Category instance because of SpaceUnit kind
compose ∷ ∀ a b c. ConversionFactor b c → ConversionFactor a b → ConversionFactor a c
compose (ConversionFactor c1) (ConversionFactor c2) = ConversionFactor (c1 * c2)

identity ∷ ∀ a. ConversionFactor a a
identity = ConversionFactor (Integers.Positive.toPositiveNumber $ Integers.Positive.reflectPos (Proxy ∷ Proxy D1))

inverse ∷ ∀ from to. ConversionFactor from to → ConversionFactor to from
inverse (ConversionFactor (Positive c)) = ConversionFactor (Positive (1.0 / c))

