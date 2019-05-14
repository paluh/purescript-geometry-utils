module Geometry.Numbers.Positive where

import Prelude

import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Pos, toInt')
import Geometry.Numbers.NonNegative (NonNegative(..))
import Type.Prelude (Proxy)

-- | You can use constructors directly on your own risk ;-)
newtype Positive = Positive Number
derive instance eqPositive :: Eq Positive
derive instance ordPositive :: Ord Positive
derive newtype instance semiringPositive ∷ Semiring Positive

instance showPositive ∷ Show Positive where
  show (Positive n) = show n

fromNumber ∷ Number → Maybe Positive
fromNumber n = if n > 0.0
  then Just (Positive n)
  else Nothing

reflectPos ∷ ∀ n. Pos n ⇒ Proxy n → Positive
reflectPos = Positive <<< Int.toNumber <<< toInt'

unsafe ∷ Number → Positive
unsafe = Positive

toNonNegative ∷ Positive → NonNegative
toNonNegative (Positive p) = NonNegative p

toNumber ∷ Positive → Number
toNumber (Positive n) = n

