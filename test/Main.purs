module Test.Main where

import Prelude

import Effect (Effect)
import Main (stripComments)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "stripComments" do
      test "nothing happens when no comments to begin with" do
        Assert.equal { type: "Interval", comments: [] } (stripComments { type: "Interval", comments: [] })
      test "all comments get discarded" do
        Assert.equal
          { comments: [] }
          (stripComments { comments: [{ offset: 0, text: "Hello" }, { offset: 20, text: "World" }] })
