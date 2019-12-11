module Geometry.Distance.ConversionFactor
  ( compose
  , conversionFactor
  , identity
  , inverse
  , unsafe
  , unsafeFromDistances
  , unsafeFromNumbers
  )
  where

import Prelude

import Data.Typelevel.Num (D1)
import Geometry.Distance.Types (ConversionFactor(..), Distance(..))
import Geometry.Integers.Positive (reflectPos, toPositiveNumber) as Integers.Positive
import Geometry.Numbers.NonNegative (toNumber) as NonNegative
import Geometry.Numbers.Positive (Positive(..))
import Type.Prelude (Proxy(..))

-- We are not able to provide Category instance because of SpaceUnit kind
compose ∷ ∀ a b c. ConversionFactor b c → ConversionFactor a b → ConversionFactor a c
compose (ConversionFactor c1) (ConversionFactor c2) = ConversionFactor (c1 * c2)

identity ∷ ∀ a. ConversionFactor a a
identity = ConversionFactor (Integers.Positive.toPositiveNumber $ Integers.Positive.reflectPos (Proxy ∷ Proxy D1))

inverse ∷ ∀ from to. ConversionFactor from to → ConversionFactor to from
inverse (ConversionFactor (Positive c)) = ConversionFactor (Positive (1.0 / c))

conversionFactor ∷ ∀ from to. { from ∷ Positive, to ∷ Positive } → ConversionFactor from to
conversionFactor { from: Positive from, to: Positive to } = ConversionFactor $ Positive (to / from)

unsafe ∷ ∀ from to. Number → ConversionFactor from to
unsafe n = ConversionFactor (Positive n)

unsafeFromNumbers ∷ ∀ from to. { from ∷ Number, to ∷ Number } → ConversionFactor from to
unsafeFromNumbers { from, to } = ConversionFactor (Positive (to / from))

unsafeFromDistances ∷ ∀ from to. Distance from → Distance to → ConversionFactor from to
unsafeFromDistances (Distance from) (Distance to) = ConversionFactor $ Positive (NonNegative.toNumber to / NonNegative.toNumber from)

