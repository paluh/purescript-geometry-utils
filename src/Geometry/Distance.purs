module Geometry.Distance where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Geometry.Positive (NonNegative(..))

foreign import kind SpaceUnit

-- | XXX: You should probably write constructor functions
-- |      which construt exactly typed version of `Distance`.
newtype Distance (unit ∷ SpaceUnit) = Distance Number
derive instance eqDistance ∷ Eq (Distance u)
derive instance ordDistance ∷ Ord (Distance u)
derive instance genericDistance ∷ Generic (Distance u) _
derive instance newtypeDistance ∷ Newtype (Distance u) _

unsafeScale ∷ ∀ u. Distance u → Number → Distance u
unsafeScale (Distance d) n = Distance (d * n)

scale ∷ ∀ u. Distance u → NonNegative → Distance u
scale (Distance d) (NonNegative n) = Distance (d * n)
