module Ast where

import Prelude
import Data.List (List)
import Data.Maybe (Maybe)

data Workout = Workout {
  name :: String,
  author :: String,
  description :: String,
  tags :: List String,
  intervals :: List Interval
}

data Interval = Interval {
  type :: String,
  duration :: Duration,
  intensity :: Intensity,
  cadence :: Maybe Cadence,
  comments :: List Comment
}

data Comment = Comment {
  offset :: Duration,
  text :: String
}

type Duration = Int

type Intensity = Number

type Cadence = Int
