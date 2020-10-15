module Zwiftout.Workout where

import Data.List (List)
import Data.Maybe (Maybe)
import Zwiftout.Intensity (Intensity)

type Workout
  = { name :: String
    , author :: String
    , description :: String
    , tags :: List String
    , intervals :: List Interval
    }

type Interval
  = { type :: String
    , duration :: Duration
    , intensity :: Intensity
    , cadence :: Maybe Cadence
    , comments :: List Comment
    }

type Comment
  = { offset :: Duration
    , text :: String
    }

type Duration
  = Int

type Cadence
  = Int
