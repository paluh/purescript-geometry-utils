module Geometry.Plane.Transformations.Affine.Isometries.Translation where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Group (class Group)
import Data.Newtype (class Newtype)
import Geometry.Distance (ConversionFactor(..), Distance, SpaceUnit)
import Geometry.Distance (unsafe) as Distance
import Geometry.Line.Transformations.Translation (Translation(..)) as Line.Transformations
import Geometry.Numbers.Positive (Positive(..))
import Geometry.Plane.Point.Types (Point(..))
import Geometry.Plane.Vector (Vector(..))
import Data.Number (sqrt)

newtype Translation (u ∷ SpaceUnit) = Translation Vector
derive instance eqTranslation ∷ Eq (Translation u)
derive instance genericTranslation ∷ Generic (Translation u) _
derive instance newtypeTranslation ∷ Newtype (Translation u) _
derive newtype instance semigroupTranslation ∷ Semigroup (Translation u)
derive newtype instance monoidTranslation ∷ Monoid (Translation u)
derive newtype instance groupTranslation ∷ Group (Translation u)

fromPoints ∷ ∀ u. Point u → Point u → Translation u
fromPoints (Point init) (Point term) = Translation $ Vector $
  { x: term.x - init.x, y: term.y - init.y }

-- | Construct translation as "position vector" which initial
-- | point lies on the origin
position ∷ ∀ u. Point u → Translation u
position (Point t) = Translation (Vector t)

position' ∷ ∀ u. Number → Number → Translation u
position' x y = Translation (Vector { x, y })

terminal ∷ ∀ u. Translation u → Point u
terminal (Translation (Vector t)) = Point t

toVector ∷ ∀ u. Translation u → Vector
toVector (Translation v) = v

_x ∷ ∀ u. Translation u → Line.Transformations.Translation u
_x (Translation (Vector r)) = Line.Transformations.Translation r.x

_y ∷ ∀ u. Translation u → Line.Transformations.Translation u
_y (Translation (Vector r)) = Line.Transformations.Translation r.y

length ∷ ∀ u. Translation u → Distance u
length (Translation (Vector { x, y })) = Distance.unsafe (sqrt (x * x + y * y))

scale ∷ ∀ u. Number → Translation u → Translation u
scale s (Translation (Vector { x, y })) = Translation (Vector { x: s * x, y: s * y })

convert ∷ ∀ from to. ConversionFactor from to → Translation from → Translation to
convert (ConversionFactor (Positive c)) (Translation (Vector { x, y })) =
  Translation (Vector { x: c * x, y: c * y })

