module Ast where

import Data.List (List)
import Data.Maybe (Maybe)

type Workout = {
  name :: String,
  author :: String,
  description :: String,
  tags :: List String,
  intervals :: List Interval
}

type Interval = {
  type :: String,
  duration :: Duration,
  intensity :: Intensity,
  cadence :: Maybe Cadence,
  comments :: List Comment
}

type Comment = {
  offset :: Duration,
  text :: String
}

type Duration = Int

type Intensity = Number

type Cadence = Int
