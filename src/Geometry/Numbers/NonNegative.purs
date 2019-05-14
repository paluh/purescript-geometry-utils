module Geometry.Numbers.NonNegative where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Ord (abs) as Ord
import Math (sqrt) as Math

newtype NonNegative = NonNegative Number
derive instance eqNonNegative :: Eq NonNegative
derive instance ordNonNegative :: Ord NonNegative
derive newtype instance semiringNonNegative ∷ Semiring NonNegative
instance showNonNegative ∷ Show NonNegative where
  show (NonNegative n) = show n

fromNumber ∷ Number → Maybe NonNegative
fromNumber n = if n >= 0.0
  then Just (NonNegative n)
  else Nothing

toNumber ∷ NonNegative → Number
toNumber (NonNegative n) = n

unsafe ∷ Number → NonNegative
unsafe = NonNegative

abs ∷ Number → NonNegative
abs n = NonNegative (Ord.abs n)

sqrt ∷ NonNegative → NonNegative
sqrt (NonNegative n) = NonNegative (Math.sqrt n)
