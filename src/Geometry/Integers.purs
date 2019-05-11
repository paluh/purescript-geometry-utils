module Geometry.Integers where

import Prelude

import Data.Foldable (class Foldable, length)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord (abs) as Ord
import Data.Semigroup.Foldable (class Foldable1)
import Data.Typelevel.Num (class Nat, class Pos, toInt')
import Geometry.Numbers (NonNegative(..))
import Type.Prelude (Proxy)

-- | 1, 2, 3, 4 ...
newtype Positive = Positive Int
derive instance eqPositive ∷ Eq Positive
derive instance ordPositive ∷ Ord Positive
derive instance genericPositive ∷ Generic Positive _
instance showPositive ∷ Show Positive where
  show (Positive n) = show n

positive ∷ Int → Maybe Positive
positive n = if n > 0
  then Just (Positive n)
  else Nothing

reflectPos ∷ ∀ n. Pos n ⇒ Proxy n → Positive
reflectPos = Positive <<< toInt'

unsafePositive ∷ Int → Positive
unsafePositive = Positive

toNatural ∷ Positive → Natural
toNatural (Positive n) = Natural n

foldable1Length ∷ ∀ a f. Foldable1 f ⇒ f a → Positive
foldable1Length f = Positive $ length f

-- | 0, 1, 2, 3 ...
newtype Natural = Natural Int
derive instance eqNatural ∷ Eq Natural
derive instance ordNatural ∷ Ord Natural
derive instance genericNatural ∷ Generic Natural _
instance showNatural ∷ Show Natural where
  show (Natural n) = show n

instance semiringNatural ∷ Semiring Natural where
  add (Natural n1) (Natural n2) = Natural (n1 + n2)
  zero = Natural 0
  mul (Natural n1) (Natural n2) = Natural (n1 * n2)
  one = Natural 1

natural ∷ Int → Maybe Natural
natural n = if n >= 0
  then Just (Natural n)
  else Nothing

reflectNat ∷ ∀ n. Nat n ⇒ Proxy n → Natural
reflectNat = Natural <<< toInt'

unsafeNatural ∷ Int → Natural
unsafeNatural = Natural

foldableLength ∷ ∀ a f. Foldable f ⇒ f a → Natural
foldableLength f = Natural $ length f

toNonNegative ∷ Natural → NonNegative
toNonNegative (Natural n) = NonNegative (toNumber n)

abs ∷ Int → Natural
abs i = Natural (Ord.abs i)
