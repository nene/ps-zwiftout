module Test.Main where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Main (collectComments, stripComments)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "stripComments" do
      test "nothing happens when no comments to begin with" do
        Assert.equal { type: "Interval", comments: Nil } (stripComments { type: "Interval", comments: Nil })
      test "all comments get discarded" do
        Assert.equal
          { comments: Nil }
          (stripComments { comments: { offset: 0, text: "Hello" } : { offset: 20, text: "World" } : Nil })
    suite "collectComments" do
      test "Does nothing with empty list" do
        Assert.equal Nil (collectComments Nil)
      test "Extracts comments of single item" do
        Assert.equal
          ({ offset: 0, text: "Hello" } : { offset: 20, text: "World" } : Nil)
          (collectComments (
            {
              type: "Interval",
              duration: 100,
              intensity: 1.0,
              cadence: Nothing,
              comments: { offset: 0, text: "Hello" } : { offset: 20, text: "World" } : Nil
            }
            : Nil
          ))
      test "changes offsets of comments of second interval" do
        Assert.equal
          ({ offset: 0, text: "Foo" } : { offset: 100, text: "Hello" } : { offset: 120, text: "World" } : Nil)
          (collectComments (
            {
              type: "Interval",
              duration: 100,
              intensity: 1.0,
              cadence: Nothing,
              comments: { offset: 0, text: "Foo" } : Nil
            }
            : {
              type: "Interval",
              duration: 10,
              intensity: 1.0,
              cadence: Nothing,
              comments: { offset: 0, text: "Hello" } : { offset: 20, text: "World" } : Nil
            }
            : Nil
          ))
