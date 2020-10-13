module Test.Main where

import Main (diagonal)
import Prelude

import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "diagonal" do
      test "3 4 5" do
        Assert.equal 5.0 (diagonal 3.0 4.0)
      test "5 12 13" do
        Assert.equal 13.0 (diagonal 5.0 12.0)
