module Zwiftout.Repeats (detectRepeats) where

import Prelude
import Data.List (List(..), all, concat, drop, length, scanl, take, takeWhile, zipWith, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Zwiftout.Intensity (Intensity(..))
import Zwiftout.PlainOrRepeatedInterval (RepeatedInterval, PlainOrRepeatedInterval(..))
import Zwiftout.Workout (Comment, Duration, Interval)

windowSize :: Int
windowSize = 2

detectRepeats :: List Interval -> List PlainOrRepeatedInterval
detectRepeats intervals = case splitAt windowSize intervals of
  Tuple Nil _ -> map Plain intervals
  Tuple _ Nil -> map Plain intervals
  Tuple (x : xs) ys -> case extractRepeatedInterval (x : xs) ys of
    Just repeatedInterval -> Repeated repeatedInterval : detectRepeats (drop repetitionLength intervals)
      where
      repetitionLength = repeatedInterval.times * windowSize
    Nothing -> Plain x : detectRepeats (drop 1 intervals)

extractRepeatedInterval :: List Interval -> List Interval -> Maybe RepeatedInterval
extractRepeatedInterval reference xs = case countRepeats reference xs of
  0 -> Nothing
  n ->
    Just
      { times: n + 1
      , intervals: map stripComments reference
      , comments: collectComments (concat (reference : take (n * windowSize) xs : Nil))
      }

countRepeats :: List Interval -> List Interval -> Int
countRepeats reference = chunk (length reference) >>> takeWhile (similar reference) >>> length

similar :: List Interval -> List Interval -> Boolean
similar xs ys = (length xs == length ys) && (all identity (zipWith similar' xs ys))
  where
  similar' :: Interval -> Interval -> Boolean
  similar' x y =
    x.type == y.type &&
    x.duration == y.duration &&
    isConstantIntensity x.intensity &&
    x.intensity == y.intensity &&
    x.cadence == y.cadence

isConstantIntensity :: Intensity -> Boolean
isConstantIntensity (ConstantIntensity _) = true
isConstantIntensity _ = false

chunk :: forall a. Int -> List a -> List (List a)
chunk 0 _ = Nil
chunk n Nil = Nil
chunk n xs = take n xs : chunk n (drop n xs)

splitAt :: forall a. Int -> List a -> Tuple (List a) (List a)
splitAt n xs = Tuple (take n xs) (drop n xs)

stripComments :: forall t. { comments :: List Comment | t } -> { comments :: List Comment | t }
stripComments interval = interval { comments = Nil }

collectComments :: List Interval -> List Comment
collectComments = collectComments' >>> concat
  where
  collectComments' :: List Interval -> List (List Comment)
  collectComments' intervals = zipWith offsetComments (intervalOffsets intervals) (map takeComments intervals)

  offsetComments :: Duration -> List Comment -> List Comment
  offsetComments amount = map (\com -> com { offset = com.offset + amount })

  -- scanl doesn't produce the first 0-offset - so we prepend 0 manually
  intervalOffsets :: List Interval -> List Duration
  intervalOffsets = map takeDuration >>> scanl (+) 0 >>> Cons 0

  takeComments i = i.comments

  takeDuration i = i.duration
