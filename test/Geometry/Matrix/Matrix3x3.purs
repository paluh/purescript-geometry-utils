module Test.Geometry.Matrix.Matrix3x3 where

import Prelude

import Geometry.Matrix.Matrix3x3 (Matrix)
import Geometry.Matrix.Matrix3x3 (array, unsafe) as Matrix3x3
import Geometry.Matrix.Matrix3x3 (unsafe) as Matrix
import Test.Unit (TestSuite)
import Test.Unit (suite, test) as Test
import Test.Unit.Assert (equal) as Assert

suite ∷ TestSuite
suite = Test.suite "Geometry.Matrix.Matrix3x3" do
  let
    m = Matrix3x3.unsafe
      [ 1.0, 2.0, 3.0
      , 4.0, 5.0, 6.0
      , 7.0, 8.0, 9.0
      ]
    m' = Matrix3x3.array m
  Test.test "multiplication by identity" do
    let
      -- Let's mark raw arrays with prime
      r = m * one
      r' = Matrix3x3.array r
    Assert.equal m' r'
  Test.test "multiplication by zero" do
    let
      z = zero ∷ Matrix
      z' = Matrix3x3.array z

      r = m * zero
      r' = Matrix3x3.array r

    Assert.equal z' r'
  Test.test "multiplication" do
    let
      r = m * m
      expected = Matrix.unsafe [ 30.0, 36.0, 42.0, 66.0, 81.0, 96.0, 102.0, 126.0, 150.0 ]
    Assert.equal expected r
