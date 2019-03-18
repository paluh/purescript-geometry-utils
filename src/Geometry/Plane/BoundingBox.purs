module Geometry.Plane.BoundingBox where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Semigroup.Foldable (maximum, minimum)
import Geometry (Distance(..))
import Geometry.Distance (kind SpaceUnit)
import Geometry.Plane.Point (Point(..), _x, _y, point)
import Geometry.Plane.Point (_x, _y) as Point
import Geometry.Plane.Transformations.Translation (Translation(..))
import Geometry.Plane.Vector (Vector(..))

type Dimentions u =
  { height ∷ Distance u
  , width ∷ Distance u
  }

-- | XXX: Maybe this whole representation should be changed to something like:
-- |  { position ∷ Point u, dimentions ∷ Dimentions u }
newtype BoundingBox (u ∷ SpaceUnit) = BoundingBox
  { height ∷ Distance u
  , width ∷ Distance u
  , x ∷ Number
  , y ∷ Number
  }
derive instance eqBoundingBox ∷ Eq (BoundingBox u)
derive instance genericBoundingBox ∷ Generic (BoundingBox u) _
derive instance newtypeBoundingBox ∷ Newtype (BoundingBox u) _

instance semigroup ∷ Semigroup (BoundingBox u) where
  append bb1 bb2 =
    let
      c1 = corners bb1
      c2 = corners bb2
      leftTop = point
        (min (_x c1.leftTop) (_x c2.leftTop))
        (min (_y c1.leftTop) (_y c2.leftTop))
      rightBottom = point
        (max (_x c1.rightBottom) (_x c2.rightBottom))
        (max (_y c1.rightBottom) (_y c2.rightBottom))
    in
      fromCorners { leftTop, rightBottom }

-- | XXX: We assume y axis direction here by some names etc.
fromCorners ∷ ∀ u. { leftTop ∷ Point u, rightBottom ∷ Point u } → BoundingBox u
fromCorners { leftTop: Point leftTop, rightBottom: Point rightBottom } = BoundingBox
  { x: leftTop.x
  , y: leftTop.y
  , width: Distance (rightBottom.x - leftTop.x)
  , height: Distance (rightBottom.y - leftTop.y)
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
  in
    fromCorners
      { leftTop: point minX minY
      , rightBottom: point maxX maxY
      }

fromBoundingCircle ∷ ∀ u. Point u → Distance u → BoundingBox u
fromBoundingCircle (Point { x, y }) (Distance r) = BoundingBox
  { x: x - r
  , y: y - r
  , height: Distance (r * 2.0)
  , width: Distance (r * 2.0)
  }

intersection ∷ ∀ u. BoundingBox u → BoundingBox u → Boolean
intersection
  (BoundingBox r1@{ height: Distance h1, width: Distance w1 })
  (BoundingBox r2@{ height: Distance h2, width: Distance w2 }) = not
    ( r2.x > r1.x + w1
    || r1.x > r2.x + w2
    || r2.y > r1.y + h1
    || r1.y > r2.y + h2
    )

corners
  ∷ ∀ u
  . BoundingBox u
  → { leftTop ∷ Point u , rightTop ∷ Point u, rightBottom ∷ Point u, leftBottom ∷ Point u }
corners (BoundingBox { x, y, height: Distance height, width: Distance width }) =
  { leftTop: point x y
  , rightTop: point (x + width) y
  , rightBottom: point (x + width) (y + height)
  , leftBottom: point x (y + height)
  }

addPadding ∷ ∀ u. Distance u → BoundingBox u → BoundingBox u
addPadding (Distance p) (BoundingBox bb@{ height: Distance h, width: Distance w}) =
  let
    height = Distance $ 2.0 * p + h
    width = Distance $ 2.0 * p + w
  in BoundingBox
    { x: bb.x - p
    , y: bb.y - p
    , height
    , width
    }

center ∷ ∀ u. BoundingBox u → Point u
center (BoundingBox { height: Distance height, width: Distance width, x, y }) =
  point ( x + width / 2.0) (y + height / 2.0)

translate ∷ ∀ u. Translation u → BoundingBox u → BoundingBox u
translate (Translation (Vector v)) (BoundingBox bb) =
  BoundingBox (bb { x = bb.x + v.x, y = bb.y + v.y })
