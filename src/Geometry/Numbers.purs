module Geometry.Numbers where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Nat, class Pos, toInt')
import Type.Prelude (Proxy)

-- | You can use constructors directly on your own risk ;-)
newtype Positive = Positive Number
derive instance eqPositive :: Eq Positive
derive instance ordPositive :: Ord Positive

positive ∷ Number → Maybe Positive
positive n = if n > 0.0
  then Just (Positive n)
  else Nothing

reflectPos ∷ ∀ n. Pos n ⇒ Proxy n → Positive
reflectPos = Positive <<< toNumber <<< toInt'

unsafePositive ∷ Number → Positive
unsafePositive = Positive

newtype Natural = Natural Number
derive instance eqNatural :: Eq Natural
derive instance ordNatural :: Ord Natural

natural ∷ Number → Maybe Natural
natural n = if n >= 0.0
  then Just (Natural n)
  else Nothing

reflectNat ∷ ∀ n. Nat n ⇒ Proxy n → Natural
reflectNat = Natural <<< toNumber <<< toInt'

unsafeNatural ∷ Number → Natural
unsafeNatural = Natural
