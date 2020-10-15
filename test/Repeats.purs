module Test.Repeats where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Zwiftout.Repeats (PlainOrRepeatedInterval(..), detectRepeats)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

testDetectRepeats :: Effect Unit
testDetectRepeats = do
  runTest do
    suite "detectRepeats" do
      test "does nothing with empty list" do
        Assert.equal Nil (detectRepeats Nil)
      test "does nothing when no interval repeats" do
        Assert.equal
          ( Plain { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
              : Plain { type: "Interval", duration: 60, intensity: 1.0, cadence: Nothing, comments: Nil }
              : Plain { type: "Interval", duration: 30, intensity: 1.2, cadence: Nothing, comments: Nil }
              : Plain { type: "Cooldown", duration: 60, intensity: 1.0, cadence: Nothing, comments: Nil }
              : Nil
          )
          ( detectRepeats
              ( { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 60, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 30, intensity: 1.2, cadence: Nothing, comments: Nil }
                  : { type: "Cooldown", duration: 60, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : Nil
              )
          )
      test "detects whole workout consisting of repetitions" do
        Assert.equal
          ( Repeated
              { times: 4
              , intervals:
                  ( { type: "Interval", duration: 120, intensity: 1.0, cadence: Nothing, comments: Nil }
                      : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                      : Nil
                  )
              , comments: Nil
              }
              : Nil
          )
          ( detectRepeats
              ( { type: "Interval", duration: 120, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : Nil
              )
          )
      test "does not detect partial repetitions" do
        Assert.equal
          ( Repeated
              { times: 2
              , intervals:
                  ( { type: "Interval", duration: 120, intensity: 1.0, cadence: Nothing, comments: Nil }
                      : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                      : Nil
                  )
              , comments: Nil
              }
              : Plain { type: "Interval", duration: 120, intensity: 1.0, cadence: Nothing, comments: Nil }
              : Nil
          )
          ( detectRepeats
              ( { type: "Interval", duration: 120, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : Nil
              )
          )
      test "detects repetitions in the middle of workout" do
        Assert.equal
          ( Plain { type: "Warmup", duration: 60, intensity: 0.75, cadence: Nothing, comments: Nil }
              : Plain { type: "Rest", duration: 120, intensity: 0.2, cadence: Nothing, comments: Nil }
              : Repeated
                  { times: 4
                  , intervals:
                      ( { type: "Interval", duration: 60, intensity: 1.0, cadence: Nothing, comments: Nil }
                          : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                          : Nil
                      )
                  , comments: Nil
                  }
              : Plain { type: "Rest", duration: 120, intensity: 0.2, cadence: Nothing, comments: Nil }
              : Plain { type: "Cooldown", duration: 60, intensity: 0.75, cadence: Nothing, comments: Nil }
              : Nil
          )
          ( detectRepeats
              ( { type: "Warmup", duration: 60, intensity: 0.75, cadence: Nothing, comments: Nil } -- TODO: use RangeIntensity here
                  : { type: "Rest", duration: 120, intensity: 0.2, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 60, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 60, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 60, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 60, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 120, intensity: 0.2, cadence: Nothing, comments: Nil }
                  : { type: "Cooldown", duration: 60, intensity: 0.75, cadence: Nothing, comments: Nil } -- TODO: use RangeIntensity here
                  : Nil
              )
          )
      test "detects multiple repetitions" do
        Assert.equal
          ( Repeated
              { times: 2
              , intervals:
                  ( { type: "Interval", duration: 60, intensity: 1.0, cadence: Nothing, comments: Nil }
                      : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                      : Nil
                  )
              , comments: Nil
              }
              : Repeated
                  { times: 2
                  , intervals:
                      ( { type: "Interval", duration: 100, intensity: 1.0, cadence: Nothing, comments: Nil }
                          : { type: "Rest", duration: 100, intensity: 0.5, cadence: Nothing, comments: Nil }
                          : Nil
                      )
                  , comments: Nil
                  }
              : Nil
          )
          ( detectRepeats
              ( { type: "Interval", duration: 60, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 60, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 100, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 100, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 100, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 100, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : Nil
              )
          )
      test "takes cadence differences into account" do
        Assert.equal
          ( Plain { type: "Interval", duration: 120, intensity: 1.0, cadence: Nothing, comments: Nil }
              : Plain { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
              : Repeated
                  { times: 2
                  , intervals:
                      ( { type: "Interval", duration: 120, intensity: 1.0, cadence: Just 100, comments: Nil }
                          : { type: "Rest", duration: 60, intensity: 0.5, cadence: Just 80, comments: Nil }
                          : Nil
                      )
                  , comments: Nil
                  }
              : Nil
          )
          ( detectRepeats
              ( { type: "Interval", duration: 120, intensity: 1.0, cadence: Nothing, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Nothing, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: 1.0, cadence: Just 100, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Just 80, comments: Nil }
                  : { type: "Interval", duration: 120, intensity: 1.0, cadence: Just 100, comments: Nil }
                  : { type: "Rest", duration: 60, intensity: 0.5, cadence: Just 80, comments: Nil }
                  : Nil
              )
          )
      test "gathers comments together" do
        Assert.equal
          ( Repeated
              { times: 2
              , intervals:
                  ( { type: "Interval", duration: 100, intensity: 1.0, cadence: Nothing, comments: Nil }
                      : { type: "Rest", duration: 100, intensity: 0.5, cadence: Nothing, comments: Nil }
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
                , intensity: 1.0
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
                    , intensity: 0.5
                    , cadence: Nothing
                    , comments:
                        ( { offset: 0, text: "Huh... have a rest" }
                            : { offset: 80, text: "Ready for next?" }
                            : Nil
                        )
                    }
                  : { type: "Interval"
                    , duration: 100
                    , intensity: 1.0
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
                    , intensity: 0.5
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
