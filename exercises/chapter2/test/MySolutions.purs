module Test.MySolutions where

import Prelude

import Math (pi, pow, sqrt)
import Data.Int (rem)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

-- Exercise 1
circleArea :: Number -> Number
circleArea r = pi * pow r 2.0

-- Exercise 2
leftoverCents :: Int -> Int
leftoverCents v = rem v 100
