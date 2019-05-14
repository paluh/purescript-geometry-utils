module Geometry.Integers.Positive where

import Prelude

import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (class Foldable1)
import Data.Typelevel.Num (class Pos, toInt')
import Geometry.Integers.Natural (Natural(..))
import Type.Prelude (Proxy)

-- | 1, 2, 3, 4 ...
newtype Positive = Positive Int
derive instance eqPositive ∷ Eq Positive
derive instance ordPositive ∷ Ord Positive
derive instance genericPositive ∷ Generic Positive _
derive newtype instance semiringPositive ∷ Semiring Positive
instance showPositive ∷ Show Positive where
  show (Positive n) = show n

positive ∷ Int → Maybe Positive
positive n = if n > 0
  then Just (Positive n)
  else Nothing

reflectPos ∷ ∀ n. Pos n ⇒ Proxy n → Positive
reflectPos = Positive <<< toInt'

unsafe ∷ Int → Positive
unsafe = Positive

toNatural ∷ Positive → Natural
toNatural (Positive n) = Natural n

foldable1Length ∷ ∀ a f. Foldable1 f ⇒ f a → Positive
foldable1Length f = Positive $ length f

