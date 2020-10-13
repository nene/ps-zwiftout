module Test.Main where

import Prelude

import Effect (Effect)
import Main (totalCadence, totalDuration)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "totalDuration" do
      test "3 + 4 = 7" do
        Assert.equal 7 (totalDuration 3 4)
      test "5 + 0 = 5" do
        Assert.equal 5 (totalDuration 5 0)

    suite "totalCadence" do
      test "3 + 4 = 7" do
        Assert.equal 7 (totalCadence 3 4)
