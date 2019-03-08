module Geometry.Distance where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

foreign import kind SpaceUnit

newtype Distance (unit ∷ SpaceUnit) = Distance Number
derive instance eqDistance ∷ Eq (Distance u)
derive instance genericDistance ∷ Generic (Distance u) _
derive instance newtypeDistance ∷ Newtype (Distance u) _

