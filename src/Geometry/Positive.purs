module Geometry.Positive where

import Prelude

import Data.Maybe (Maybe(..))

-- | You can use constructors directly on your own risk ;-)

newtype Positive = Positive Number
derive instance eqPositive :: Eq Positive
derive instance ordPositive :: Ord Positive

positive ∷ Number → Maybe Positive
positive n = if n > 0.0
  then Just (Positive n)
  else Nothing

newtype NonNegative = NonNegative Number
derive instance eqNonNegative :: Eq NonNegative
derive instance ordNonNegative :: Ord NonNegative

nonNegative ∷ Number → Maybe NonNegative
nonNegative n = if n >= 0.0
  then Just (NonNegative n)
  else Nothing
