module Test.Main where

import Prelude

import Effect (Effect)
import Test.Geometry.Matrix.Matrix3x3 (suite) as Matrix3x3
import Test.Geometry.Plane.Transformations.Affine as Affine
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest $ do
  Matrix3x3.suite
  Affine.suite

