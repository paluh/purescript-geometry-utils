module Geometry.Utils where

import Prelude

import Data.Newtype (ala)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Semigroup.Foldable (class Foldable1, foldMap1)

maximum1 ∷ ∀ f a. Ord a ⇒ Foldable1 f ⇒ f a → a
maximum1 = ala Max foldMap1

minimum1 ∷ ∀ f a. Ord a ⇒ Foldable1 f ⇒ f a → a
minimum1 = ala Min foldMap1

