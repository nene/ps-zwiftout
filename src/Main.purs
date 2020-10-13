module Main where

import Prelude
import Ast (Duration, Cadence)

totalDuration :: Duration -> Duration -> Duration
totalDuration a b = a + b

totalCadence :: Cadence -> Cadence -> Cadence
totalCadence a b = a + b
