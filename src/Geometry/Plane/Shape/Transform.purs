module Geometry.Plane.Shape.Transform where

-- import Prelude
-- 
-- import Data.Group (ginverse)
-- import Geometry (Angle(..), toRadians)
-- import Geometry.Plane.Point (Point)
-- import Geometry.Plane.Point (transform) as Point
-- import Geometry.Plane.Shape (Shape(..), center)
-- import Geometry.Plane.Transform (rotation, scaling, translation) as Matrix
-- import Geometry.Plane.Vector (Vector(..), vector)
-- import Math (Radians)
-- 
-- position ∷ Point → Shape → Shape
-- position point shape = translate ((Vector point) <> back) shape
--   where
--     back = ginverse <<< Vector <<< center $ shape
-- 
-- translate ∷ trans → Shape → Shape
-- translate vector =
--   let
--     m = Matrix.translation vector
--     tp = Point.transform m
--   in
--     case _ of
--       Circle { center: c, radius } → Circle
--         { center: tp c, radius }
--       Ellipse { center: c, rx, ry, rotation } → Ellipse
--         { center: tp c, rx, ry, rotation }
--       Quadrilateral { center: c, p1, p2, p3, p4 } → Quadrilateral
--         { center: tp c, p1: tp p1 , p2: tp p2 , p3: tp p3 , p4: tp p4 }
--       Triangle { center: c, p1, p2, p3 } → Triangle
--         { center: tp c, p1: tp p1, p2: tp p2, p3: tp p3 }
-- 
-- scale ∷ Number → Shape → Shape
-- scale rate =
--   case _ of
--     Circle { center: c, radius } → Circle
--       { center: c, radius: rate * radius }
--     Ellipse { center: c, rx, ry, rotation } → Ellipse
--       { center: c, rx: rate * rx, ry: rate * ry, rotation }
--     Quadrilateral { center: c, p1, p2, p3, p4 } →
--       let
--         m = matrix c
--         tp = Point.transform m
--       in
--         Quadrilateral { center: tp c, p1: tp p1, p2: tp p2, p3: tp p3, p4: tp p4}
--     Triangle { center: c, p1, p2, p3 } →
--       let
--         m = matrix c
--         tp = Point.transform m
--       in
--         Triangle { center: tp c, p1: tp p1, p2: tp p2, p3: tp p3}
--   where
--     matrix c =
--       Matrix.translation (vector c.x c.y)
--       * Matrix.scaling (Vector { x: rate, y: rate })
--       * Matrix.translation (vector (-c.x) (-c.y))
-- 
-- rotate ∷ Angle → Shape → Shape
-- rotate angle = case _ of
--     Circle r → Circle r
--     Ellipse { center: c, rx, ry, rotation } → Ellipse
--       { center: c, rx, ry, rotation: toRadians ((Radians rotation) <> angle) }
--     Quadrilateral { center: c, p1, p2, p3, p4 } →
--       let
--         m = matrix c
--         tp = Point.transform m
--       in
--         Quadrilateral { center: c, p1: tp p1, p2: tp p2, p3: tp p3, p4: tp p4}
--     Triangle { center: c, p1, p2, p3 } →
--       let
--         m = matrix c
--         tp = Point.transform m
--       in
--         Triangle { center: c, p1: tp p1, p2: tp p2, p3: tp p3}
--   where
--     matrix c =
--       Matrix.translation (vector c.x c.y)
--       * Matrix.rotation angle
--       * Matrix.translation (vector (-c.x) (-c.y))
