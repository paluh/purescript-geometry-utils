module Geometry.Plane.CoordinateSystem where

import Geometry.Distance (SpaceUnit)
import Geometry.Plane.Point (Point)

class CoordinateSystem p (u :: SpaceUnit) | p -> u where
  fromCartesian :: Point u -> p
  toCartesian :: p -> Point u
