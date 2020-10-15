module Zwiftout.PlainOrRepeatedInterval where

import Prelude
import Data.List (List)
import Zwiftout.Workout (Interval, Comment)

type RepeatedInterval
  = { times :: Int
    , intervals :: List Interval
    , comments :: List Comment
    }

data PlainOrRepeatedInterval
  = Plain Interval
  | Repeated RepeatedInterval

instance eqPlainOrRepeatedInterval :: Eq PlainOrRepeatedInterval where
  eq (Plain a) (Plain b) = a == b
  eq (Repeated a) (Repeated b) = a == b
  eq (Plain a) (Repeated b) = false
  eq (Repeated a) (Plain b) = false

instance showPlainOrRepeatedInterval :: Show PlainOrRepeatedInterval where
  show (Plain a) = "(Plain " <> show a <> ")"
  show (Repeated a) = "(Repeated " <> show a <> ")"
