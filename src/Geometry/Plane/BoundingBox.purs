module Geometry.Plane.BoundingBox
  ( BoundingBox(..)
  , Percent(..)
  , addPadding
  , addPadding'
  , addPadding''
  , addPercentPadding
  , addPercentPadding'
  , addPercentPadding''
  , aspectRatio
  , center
  , corners
  , convert
  , dimensions
  , fromCorners
  , fromPoints
  , fromBoundingCircle
  , intersection
  , originCentered
  , overlap
  , Padding
  , padding'
  , padding''
  , point
  , translate
  , unsafe
  , module Exports
  )
  where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype)
import Data.Semigroup.Foldable (maximum, minimum)
import Geometry (Distance(..))
import Geometry.Distance (ConversionFactor(..), fromNonNegative, SpaceUnit)
import Geometry.Distance (convert, fromNonNegative, scale, toNumber) as Distance
import Geometry.Numbers.NonNegative (NonNegative(..))
import Geometry.Numbers.NonNegative (abs, fromNumber, unsafe) as NonNegative
import Geometry.Numbers.Positive (Positive(..))
import Geometry.Plane.BoundingBox.AspectRatio (AspectRatio)
import Geometry.Plane.BoundingBox.Dimensions (Dimensions)
import Geometry.Plane.BoundingBox.Dimensions (Dimensions) as Exports
import Geometry.Plane.BoundingBox.Dimensions (aspectRatio) as Dimensions
import Geometry.Plane.Point (Point(..), _x, _y)
import Geometry.Plane.Point (_x, _y, point) as Point
import Geometry.Plane.Transformations.Affine.Isometries.Translation (Translation(..))
import Geometry.Plane.Vector (Vector(..))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- | XXX: Maybe this whole representation should be changed to something like:
-- |  { position ∷ Point u, dimensions ∷ Dimensions u }
newtype BoundingBox (u ∷ SpaceUnit) = BoundingBox
  { height ∷ Distance u
  , width ∷ Distance u
  , x ∷ Number
  , y ∷ Number
  }
derive instance eqBoundingBox ∷ Eq (BoundingBox u)
derive instance genericBoundingBox ∷ Generic (BoundingBox u) _
derive instance newtypeBoundingBox ∷ Newtype (BoundingBox u) _

unsafe ∷ ∀ u. { x ∷ Number, y ∷ Number, height ∷ Number, width ∷ Number } → BoundingBox u
unsafe = unsafeCoerce

instance semigroup ∷ Semigroup (BoundingBox u) where
  append bb1 bb2 =
    let
      c1 = corners bb1
      c2 = corners bb2
      leftTop = Point.point
        (min (_x c1.leftTop) (_x c2.leftTop))
        (min (_y c1.leftTop) (_y c2.leftTop))
      rightBottom = Point.point
        (max (_x c1.rightBottom) (_x c2.rightBottom))
        (max (_y c1.rightBottom) (_y c2.rightBottom))
    in
      unsafePartial $ fromJust $ fromCorners { leftTop, rightBottom }

fromCorners ∷ ∀ u. { leftTop ∷ Point u, rightBottom ∷ Point u } → Maybe (BoundingBox u)
fromCorners { leftTop: Point leftTop, rightBottom: Point rightBottom } = do
  height ← Distance.fromNonNegative <$> NonNegative.fromNumber (rightBottom.y - leftTop.y)
  width ← Distance.fromNonNegative <$> NonNegative.fromNumber (rightBottom.x - leftTop.x)
  pure $ BoundingBox
    { x: leftTop.x
    , y: leftTop.y
    , width
    , height
    }

fromPoints ∷ ∀ u. NonEmptyArray (Point u) → BoundingBox u
fromPoints points =
  let
    xs = map Point._x points
    minX = minimum xs
    maxX = maximum xs
    ys = map Point._y points
    minY = minimum ys
    maxY = maximum ys
  in BoundingBox
    { x: minX
    , y: minY
    , width: fromNonNegative (NonNegative.abs (maxX - minX))
    , height: fromNonNegative (NonNegative.abs (maxY - minY))
    }

fromBoundingCircle ∷ ∀ u. Point u → Distance u → BoundingBox u
fromBoundingCircle (Point { x, y }) (Distance r@(NonNegative rv)) = BoundingBox
  { x: x - rv
  , y: y - rv
  , height: Distance (r * (NonNegative 2.0))
  , width: Distance (r * (NonNegative 2.0))
  }

originCentered ∷ ∀ u. { height ∷ Distance u, width ∷ Distance u } → BoundingBox u
originCentered { height, width } =
  let
    Distance (NonNegative h) = height
    Distance (NonNegative w) = width
  in BoundingBox
    { x: w / -2.0
    , y: h / -2.0
    , height
    , width
    }

-- intersect = meet or overlap
intersection ∷ ∀ u. BoundingBox u → BoundingBox u → Boolean
intersection
  (BoundingBox r1@{ height: Distance (NonNegative h1), width: Distance (NonNegative w1) })
  (BoundingBox r2@{ height: Distance (NonNegative h2), width: Distance (NonNegative w2) }) = not
    ( r2.x > r1.x + w1
    || r1.x > r2.x + w2
    || r2.y > r1.y + h1
    || r1.y > r2.y + h2
    )

overlap ∷ ∀ u. BoundingBox u → BoundingBox u → Boolean
overlap
  (BoundingBox r1@{ height: Distance (NonNegative h1), width: Distance (NonNegative w1) })
  (BoundingBox r2@{ height: Distance (NonNegative h2), width: Distance (NonNegative w2) }) = not
    ( r2.x >= r1.x + w1
    || r1.x >= r2.x + w2
    || r2.y >= r1.y + h1
    || r1.y >= r2.y + h2
    )

-- | XXX: We should probably reorganize module structure and use quadrilateral here...
-- |      or just rename fields...
corners
  ∷ ∀ u
  . BoundingBox u
  → { leftTop ∷ Point u , rightTop ∷ Point u, rightBottom ∷ Point u, leftBottom ∷ Point u }
corners (BoundingBox { x, y, height: Distance (NonNegative height), width: Distance (NonNegative width) }) =
  { leftTop: Point.point x y
  , rightTop: Point.point (x + width) y
  , rightBottom: Point.point (x + width) (y + height)
  , leftBottom: Point.point x (y + height)
  }

type Padding u =
  { bottom ∷ u
  , left ∷ u
  , right ∷ u
  , top ∷ u
  }

padding' ∷ ∀ u. { horizontal ∷ u, vertical ∷ u } → Padding u
padding' { horizontal, vertical } = { bottom: vertical, left: horizontal, right: horizontal, top: vertical }

padding'' ∷ ∀ u. u → Padding u
padding'' p = { bottom: p, left: p, right: p, top: p }

addPadding ∷ ∀ u. Padding (Distance u) → BoundingBox u → BoundingBox u
addPadding { bottom, left, right, top } (BoundingBox { height, width, x, y }) =
  let
    height' = top <> height <> bottom
    width' = left <> width <> right
    x' = x - Distance.toNumber left
    y' = y - Distance.toNumber top
  in BoundingBox
    { x: x'
    , y: y'
    , height: height'
    , width: width'
    }

addPadding' ∷ ∀ u. { horizontal ∷ Distance u, vertical ∷ Distance u } → BoundingBox u → BoundingBox u
addPadding' { horizontal: hp, vertical: vp } =
  addPadding { bottom: hp, left: vp, right: vp, top: hp }

addPadding'' ∷ ∀ u. Distance u → BoundingBox u → BoundingBox u
addPadding'' p =
  addPadding { bottom: p, left: p, right: p, top: p }

newtype Percent = Percent NonNegative

scaleByPercent ∷ ∀ u. Percent → Distance u → Distance u
scaleByPercent (Percent p) = flip Distance.scale (p * NonNegative.unsafe 0.01)

addPercentPadding ∷ ∀ u. Padding Percent → BoundingBox u → BoundingBox u
addPercentPadding { bottom, left, right, top } boundingBox@(BoundingBox { height, width }) =
  addPadding padding boundingBox
  where
    padding =
      { bottom: scaleByPercent bottom height
      , left: scaleByPercent left width
      , right: scaleByPercent right width
      , top: scaleByPercent top height
      }

addPercentPadding' ∷ ∀ u. { horizontal ∷ Percent, vertical ∷ Percent } → BoundingBox u → BoundingBox u
addPercentPadding' { horizontal: hp, vertical: vp } =
  addPercentPadding { bottom: hp, left: vp, right: vp, top: hp }

addPercentPadding'' ∷ ∀ u. Percent → BoundingBox u → BoundingBox u
addPercentPadding'' p =
  addPercentPadding { bottom: p, left: p, right: p, top: p }

center ∷ ∀ u. BoundingBox u → Point u
center (BoundingBox { height: Distance (NonNegative height), width: Distance (NonNegative width), x, y }) =
  Point.point ( x + width / 2.0) (y + height / 2.0)

translate ∷ ∀ u. Translation u → BoundingBox u → BoundingBox u
translate (Translation (Vector v)) (BoundingBox bb) =
  BoundingBox (bb { x = bb.x + v.x, y = bb.y + v.y })

convert ∷ ∀ from to. ConversionFactor from to → BoundingBox from → BoundingBox to
convert c@(ConversionFactor (Positive cv)) (BoundingBox { x, y, height, width }) = BoundingBox
  { x: cv * x
  , y: cv * y
  , height: Distance.convert c height
  , width: Distance.convert c width
  }

dimensions ∷ ∀ u. BoundingBox u → Dimensions u
dimensions (BoundingBox r) = { height: r.height, width: r.width }

point ∷ ∀ u. BoundingBox u → Point u
point (BoundingBox { x, y }) = Point.point x y

aspectRatio ∷ ∀ u. BoundingBox u → AspectRatio
aspectRatio = dimensions >>> Dimensions.aspectRatio
