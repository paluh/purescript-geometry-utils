module Test.Geometry.Matrix.Matrix2x2 where

import Prelude

import Geometry (kind SpaceUnit)
import Geometry.Matrix.Matrix2x2 (Matrix)
import Geometry.Matrix.Matrix2x2 (array, unsafe) as Matrix2x2
import Geometry.Matrix.Matrix2x2 (unsafe) as Matrix
import Test.Unit (TestSuite)
import Test.Unit (suite, test) as Test
import Test.Unit.Assert (equal) as Assert

suite ∷ TestSuite
suite = Test.suite "Geometry.Matrix.Matrix2x2" do
  let
    m = Matrix2x2.unsafe
      [ 1.0, 2.0
      , 3.0, 4.0
      ]
    m' = Matrix2x2.array m
  Test.test "multiplication by identity" do
    let
      -- Let's mark raw arrays with prime
      r = m * one
      r' = Matrix2x2.array r
    Assert.equal m' r'
  Test.test "multiplication by zero" do
    let
      z = zero ∷ Matrix
      z' = Matrix2x2.array z

      r = m * zero
      r' = Matrix2x2.array r

    Assert.equal z' r'
  Test.test "multiplication" do
    let
      r = m * m
      expected = Matrix.unsafe [ 30.0, 36.0, 42.0, 66.0, 81.0, 96.0, 102.0, 126.0, 150.0 ]
    Assert.equal expected r
