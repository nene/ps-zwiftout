module Main where

import Prelude

import Ast (Comment, Duration, Interval)
import Data.List (List(..), concat, scanl, zipWith)

stripComments :: forall t. { comments :: List Comment | t } -> { comments :: List Comment | t }
stripComments interval = interval { comments = Nil }

collectComments :: List Interval -> List Comment
collectComments = collectComments' >>> concat where
    collectComments' :: List Interval -> List (List Comment)
    collectComments' intervals = zipWith offsetComments (intervalOffsets intervals) (map takeComments intervals)

    offsetComments :: Duration -> List Comment -> List Comment
    offsetComments amount = map (\ com -> com { offset = com.offset + amount } )

    -- scanl doesn't produce the first 0-offset - so we prepend 0 manually
    intervalOffsets :: List Interval -> List Duration
    intervalOffsets = map takeDuration >>> scanl (+) 0 >>> Cons 0

    takeComments i = i.comments
    takeDuration i = i.duration
