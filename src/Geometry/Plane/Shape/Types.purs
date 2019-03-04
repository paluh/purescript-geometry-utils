module Geometry.Plane.Shape.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Geometry.Plane.Point (Point)
import Geometry.SpaceUnit (kind SpaceUnit)
import Math (Radians)

-- | XXX: Probably we can drop `Circle` and use only `Ellipse`
data Shape (u ∷ SpaceUnit)
  = Circle { center ∷ Point u, radius ∷  Number }
  | Ellipse { center ∷ Point u, rx ∷ Number , ry ∷ Number , rotation ∷ Radians }
  | Quadrilateral { center ∷ Point u, p1 ∷ Point u, p2 ∷ Point u, p3 ∷ Point u, p4 ∷ Point u }
  | Triangle { center ∷ Point u, p1 ∷ Point u, p2 ∷ Point u, p3 ∷ Point u }
derive instance eqShape ∷ Eq (Shape u)
derive instance genericShape ∷ Generic (Shape u) _
instance encodeShape ∷ EncodeJson (Shape u) where
  encodeJson = genericEncodeJson
instance decodeDesign ∷ DecodeJson (Shape u) where
  decodeJson = genericDecodeJson
