module Zwiftout.Intensity where

import Prelude

data Intensity
  = ConstantIntensity Number
  | RangeIntensity Number Number
  | FreeIntensity

instance eqIntensity :: Eq Intensity where
    eq FreeIntensity FreeIntensity = true
    eq (ConstantIntensity a) (ConstantIntensity b) = a == b
    eq (RangeIntensity a1 a2) (RangeIntensity b1 b2) = a1 == b1 && a2 == b2
    eq _ _ = false

instance showIntensity :: Show Intensity where
    show FreeIntensity = "FreeIntensity"
    show (ConstantIntensity a) = "(ConstantIntensity " <> show a <> ")"
    show (RangeIntensity a b) = "(RangeIntensity " <> show a <> " " <> show b <> ")"
