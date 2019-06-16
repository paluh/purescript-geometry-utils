module Test.Geometry.Plane.Transformations.Affine where

import Prelude

import Geometry (kind SpaceUnit)
import Geometry.Plane.Transformations.Affine.Matrix (unsafe) as Matrix
import Test.Unit (TestSuite)
import Test.Unit (suite, test) as Test
import Test.Unit.Assert (equal) as Assert

foreign import data Scene ∷ SpaceUnit

suite ∷ TestSuite
suite = Test.suite "Geometry.Plane.Transformations.Affine" do
  let
    m = Matrix.unsafe
      [ 1.0, 2.0, 3.0
      , 4.0, 5.0, 6.0
      , 0.0, 0.0, 1.0
      ]
  Test.test "multiplication by one" do
    let
      r = m <> mempty
    Assert.equal m r
  Test.test "multiplication" do
    let
      r = m <> m
      expected = Matrix.unsafe
        [ 9.0, 12.0, 18.0, 24.0, 33.0, 48.0, 0.0, 0.0, 1.0 ]
    Assert.equal expected r
