module Test.Repeats where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Zwiftout.Intensity (Intensity(..))
import Zwiftout.Repeats (PlainOrRepeatedInterval(..), detectRepeats)

testDetectRepeats :: Effect Unit
testDetectRepeats = do
  runTest do
    suite "detectRepeats" do
      test "does nothing with empty list" do
        Assert.equal Nil (detectRepeats Nil)
      test "does nothing when no interval repeats" do
        Assert.equal
          ( Plain { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
              : Plain { type: "Interval", duration: 60, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
              : Plain { type: "Interval", duration: 30, intensity: ConstantIntensity 1.2, cadence: Nothing, comments: Nil }
              : Plain { type: "Cooldown", duration: 60, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
              : Nil
          )
          ( detectRepeats
              ( { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 60, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 30, intensity: ConstantIntensity 1.2, cadence: Nothing, comments: Nil }
                  : { type: "Cooldown", duration: 60, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : Nil
              )
          )
      test "detects whole workout consisting of repetitions" do
        Assert.equal
          ( Repeated
              { times: 4
              , intervals:
                  ( { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                      : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                      : Nil
                  )
              , comments: Nil
              }
              : Nil
          )
          ( detectRepeats
              ( { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : Nil
              )
          )
      test "does not detect partial repetitions" do
        Assert.equal
          ( Repeated
              { times: 2
              , intervals:
                  ( { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                      : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                      : Nil
                  )
              , comments: Nil
              }
              : Plain { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
              : Nil
          )
          ( detectRepeats
              ( { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : Nil
              )
          )
      test "detects repetitions in the middle of workout" do
        Assert.equal
          ( Plain { type: "Warmup", duration: 60, intensity: RangeIntensity 0.1 0.2, cadence: Nothing, comments: Nil }
              : Plain { type: "Rest", duration: 120, intensity: ConstantIntensity 0.2, cadence: Nothing, comments: Nil }
              : Repeated
                  { times: 4
                  , intervals:
                      ( { type: "Interval", duration: 60, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                          : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                          : Nil
                      )
                  , comments: Nil
                  }
              : Plain { type: "Rest", duration: 120, intensity: ConstantIntensity 0.2, cadence: Nothing, comments: Nil }
              : Plain { type: "Cooldown", duration: 60, intensity: RangeIntensity 0.2 0.1, cadence: Nothing, comments: Nil }
              : Nil
          )
          ( detectRepeats
              ( { type: "Warmup", duration: 60, intensity: RangeIntensity 0.1 0.2, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 120, intensity: ConstantIntensity 0.2, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 60, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 60, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 60, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 60, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 120, intensity: ConstantIntensity 0.2, cadence: Nothing, comments: Nil }
                  : { type: "Cooldown", duration: 60, intensity: RangeIntensity 0.2 0.1, cadence: Nothing, comments: Nil }
                  : Nil
              )
          )
      test "detects multiple repetitions" do
        Assert.equal
          ( Repeated
              { times: 2
              , intervals:
                  ( { type: "Interval", duration: 60, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                      : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                      : Nil
                  )
              , comments: Nil
              }
              : Repeated
                  { times: 2
                  , intervals:
                      ( { type: "Interval", duration: 100, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                          : { type: "Rest", duration: 100, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                          : Nil
                      )
                  , comments: Nil
                  }
              : Nil
          )
          ( detectRepeats
              ( { type: "Interval", duration: 60, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 60, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 100, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 100, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 100, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 100, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : Nil
              )
          )
      test "takes cadence differences into account" do
        Assert.equal
          ( Plain { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
              : Plain { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
              : Repeated
                  { times: 2
                  , intervals:
                      ( { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Just 100, comments: Nil }
                          : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Just 80, comments: Nil }
                          : Nil
                      )
                  , comments: Nil
                  }
              : Nil
          )
          ( detectRepeats
              ( { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Just 100, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Just 80, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: ConstantIntensity 1.0, cadence: Just 100, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: ConstantIntensity 0.5, cadence: Just 80, comments: Nil }
                  : Nil
              )
          )
      test "does not consider range-intensity-intervals to be repeatable" do
        Assert.equal
          ( Plain { type: "Warmup", duration: 30, intensity: RangeIntensity 0.5 1.0, cadence: Nothing, comments: Nil }
              : Plain { type: "Cooldown", duration: 60, intensity: RangeIntensity 1.0 0.5, cadence: Nothing, comments: Nil }
              : Plain { type: "Warmup", duration: 30, intensity: RangeIntensity 0.5 1.0, cadence: Nothing, comments: Nil }
              : Plain { type: "Cooldown", duration: 60, intensity: RangeIntensity 1.0 0.5, cadence: Nothing, comments: Nil }
              : Plain { type: "Warmup", duration: 30, intensity: RangeIntensity 0.5 1.0, cadence: Nothing, comments: Nil }
              : Plain { type: "Cooldown", duration: 60, intensity: RangeIntensity 1.0 0.5, cadence: Nothing, comments: Nil }
              : Nil
          )
          ( detectRepeats
              ( { type: "Warmup", duration: 30, intensity: RangeIntensity 0.5 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Cooldown", duration: 60, intensity: RangeIntensity 1.0 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Warmup", duration: 30, intensity: RangeIntensity 0.5 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Cooldown", duration: 60, intensity: RangeIntensity 1.0 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Warmup", duration: 30, intensity: RangeIntensity 0.5 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Cooldown", duration: 60, intensity: RangeIntensity 1.0 0.5, cadence: Nothing, comments: Nil }
                  : Nil
              )
          )
      test "does not consider free-intensity-intervals to be repeatable" do
        Assert.equal
          ( Plain { type: "FreeRide", duration: 30, intensity: FreeIntensity, cadence: Nothing, comments: Nil }
              : Plain { type: "FreeRide", duration: 60, intensity: FreeIntensity, cadence: Nothing, comments: Nil }
              : Plain { type: "FreeRide", duration: 30, intensity: FreeIntensity, cadence: Nothing, comments: Nil }
              : Plain { type: "FreeRide", duration: 60, intensity: FreeIntensity, cadence: Nothing, comments: Nil }
              : Plain { type: "FreeRide", duration: 30, intensity: FreeIntensity, cadence: Nothing, comments: Nil }
              : Plain { type: "FreeRide", duration: 60, intensity: FreeIntensity, cadence: Nothing, comments: Nil }
              : Nil
          )
          ( detectRepeats
              ( { type: "FreeRide", duration: 30, intensity: FreeIntensity, cadence: Nothing, comments: Nil }
                  : { type: "FreeRide", duration: 60, intensity: FreeIntensity, cadence: Nothing, comments: Nil }
                  : { type: "FreeRide", duration: 30, intensity: FreeIntensity, cadence: Nothing, comments: Nil }
                  : { type: "FreeRide", duration: 60, intensity: FreeIntensity, cadence: Nothing, comments: Nil }
                  : { type: "FreeRide", duration: 30, intensity: FreeIntensity, cadence: Nothing, comments: Nil }
                  : { type: "FreeRide", duration: 60, intensity: FreeIntensity, cadence: Nothing, comments: Nil }
                  : Nil
              )
          )
      test "gathers comments together" do
        Assert.equal
          ( Repeated
              { times: 2
              , intervals:
                  ( { type: "Interval", duration: 100, intensity: ConstantIntensity 1.0, cadence: Nothing, comments: Nil }
                      : { type: "Rest", duration: 100, intensity: ConstantIntensity 0.5, cadence: Nothing, comments: Nil }
                      : Nil
                  )
              , comments:
                  ( { offset: 0, text: "Let's start" }
                      : { offset: 20, text: "Stay strong!" }
                      : { offset: 90, text: "Finish it!" }
                      : { offset: 100, text: "Huh... have a rest" }
                      : { offset: 180, text: "Ready for next?" }
                      : { offset: 200, text: "Bring it on again!" }
                      : { offset: 250, text: "Half way" }
                      : { offset: 290, text: "Almost there!" }
                      : { offset: 330, text: "Wow... you did it!" }
                      : { offset: 340, text: "Nice job." }
                      : { offset: 350, text: "Until next time..." }
                      : Nil
                  )
              }
              : Nil
          )
          ( detectRepeats
              ( { type: "Interval"
                , duration: 100
                , intensity: ConstantIntensity 1.0
                , cadence: Nothing
                , comments:
                    ( { offset: 0, text: "Let's start" }
                        : { offset: 20, text: "Stay strong!" }
                        : { offset: 90, text: "Finish it!" }
                        : Nil
                    )
                }
                  : { type: "Rest"
                    , duration: 100
                    , intensity: ConstantIntensity 0.5
                    , cadence: Nothing
                    , comments:
                        ( { offset: 0, text: "Huh... have a rest" }
                            : { offset: 80, text: "Ready for next?" }
                            : Nil
                        )
                    }
                  : { type: "Interval"
                    , duration: 100
                    , intensity: ConstantIntensity 1.0
                    , cadence: Nothing
                    , comments:
                        ( { offset: 0, text: "Bring it on again!" }
                            : { offset: 50, text: "Half way" }
                            : { offset: 90, text: "Almost there!" }
                            : Nil
                        )
                    }
                  : { type: "Rest"
                    , duration: 100
                    , intensity: ConstantIntensity 0.5
                    , cadence: Nothing
                    , comments:
                        ( { offset: 30, text: "Wow... you did it!" }
                            : { offset: 40, text: "Nice job." }
                            : { offset: 50, text: "Until next time..." }
                            : Nil
                        )
                    }
                  : Nil
              )
          )
