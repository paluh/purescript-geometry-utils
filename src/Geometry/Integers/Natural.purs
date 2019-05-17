module Geometry.Integers.Natural where

import Prelude

import Data.Foldable (class Foldable, length)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord (abs) as Ord
import Data.Typelevel.Num (class Nat, toInt')
import Geometry.Numbers.NonNegative (NonNegative(..))
import Type.Prelude (Proxy)

-- | 0, 1, 2, 3 ...
newtype Natural = Natural Int
derive instance eqNatural ∷ Eq Natural
derive instance ordNatural ∷ Ord Natural
derive instance genericNatural ∷ Generic Natural _
derive newtype instance semiringNatural ∷ Semiring Natural
instance showNatural ∷ Show Natural where
  show (Natural n) = show n

natural ∷ Int → Maybe Natural
natural n = if n >= 0
  then Just (Natural n)
  else Nothing

reflectNat ∷ ∀ n. Nat n ⇒ Proxy n → Natural
reflectNat = Natural <<< toInt'

unsafeNatural ∷ Int → Natural
unsafeNatural = Natural

toInt ∷ Natural → Int
toInt (Natural n) = n

foldableLength ∷ ∀ a f. Foldable f ⇒ f a → Natural
foldableLength f = Natural $ length f

toNonNegative ∷ Natural → NonNegative
toNonNegative (Natural n) = NonNegative (toNumber n)

abs ∷ Int → Natural
abs i = Natural (Ord.abs i)
