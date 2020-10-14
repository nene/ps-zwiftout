module Test.Main where

import Prelude

import Effect (Effect)
import Test.Repeats (testDetectRepeats)

main :: Effect Unit
main = do
  testDetectRepeats
