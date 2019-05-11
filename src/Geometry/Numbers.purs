module Geometry.Numbers where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord (abs) as Ord
import Data.Typelevel.Num (class Pos, toInt')
import Math (sqrt) as Math
import Type.Prelude (Proxy)

-- | You can use constructors directly on your own risk ;-)
newtype Positive = Positive Number
derive instance eqPositive :: Eq Positive
derive instance ordPositive :: Ord Positive
instance showPositive ∷ Show Positive where
  show (Positive n) = show n

positive ∷ Number → Maybe Positive
positive n = if n > 0.0
  then Just (Positive n)
  else Nothing

reflectPos ∷ ∀ n. Pos n ⇒ Proxy n → Positive
reflectPos = Positive <<< toNumber <<< toInt'

unsafePositive ∷ Number → Positive
unsafePositive = Positive

toNonNegative ∷ Positive → NonNegative
toNonNegative (Positive p) = NonNegative p

positiveToNumber ∷ Positive → Number
positiveToNumber (Positive n) = n

newtype NonNegative = NonNegative Number
derive instance eqNonNegative :: Eq NonNegative
derive instance ordNonNegative :: Ord NonNegative
instance showNonNegative ∷ Show NonNegative where
  show (NonNegative n) = show n

instance semiringNonNegative ∷ Semiring NonNegative where
  add (NonNegative n1) (NonNegative n2) = NonNegative (n1 + n2)
  zero = NonNegative 0.0
  mul (NonNegative n1) (NonNegative n2) = NonNegative (n1 * n2)
  one = NonNegative 1.0

nonNegative ∷ Number → Maybe NonNegative
nonNegative n = if n >= 0.0
  then Just (NonNegative n)
  else Nothing

unsafeNonNegative ∷ Number → NonNegative
unsafeNonNegative = NonNegative

nonNegativeToNumber ∷ NonNegative → Number
nonNegativeToNumber (NonNegative n) = n

abs ∷ Number → NonNegative
abs n = NonNegative (Ord.abs n)

sqrt ∷ NonNegative → NonNegative
sqrt (NonNegative n) = NonNegative (Math.sqrt n)
