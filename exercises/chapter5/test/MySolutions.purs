module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Person (Person)
import Data.Picture (Bounds, Picture, Point, Shape(..), emptyBounds, getCenter, origin)
import Data.Maybe (Maybe(..))
import Math as Math

-- Exercise 1.1
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Exercise 1.2
binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n < k = 0
             | otherwise = (factorial n) / ((factorial k) * (factorial (n - k)))

-- Exercise 1.3
pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)

-- Exercise 2.1
sameCity :: Person -> Person -> Boolean
sameCity { address: { city: city1 } } { address: { city: city2 } } = city1 == city2

-- Exercise 2.2
-- Text exercise.

-- Exercise 2.3
fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [e] = e
fromSingleton default _ = default

-- Exercise 3.1
circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

-- Exercise 3.2
scaleShape :: Number -> Shape -> Shape
scaleShape factor (Circle c r) = Circle c $ (r * factor)
scaleShape factor (Rectangle c w h) = Rectangle c (w * factor) (h * factor)
scaleShape factor (Line s e) = Line (s * scale) (e * scale)
  where
  scale :: Point
  scale = { x: factor, y: factor }
scaleShape _ (Text c s) = Text c s
scaleShape factor (Clipped p c w h) = Clipped p c (w * factor) (h * factor)

centerShape :: Shape -> Shape
centerShape (Circle _ r) = Circle origin r
centerShape (Rectangle _ w h) = Rectangle origin w h
centerShape line@(Line s e) = Line (s - delta) (e - delta)
  where
  delta :: Point
  delta = getCenter line
centerShape (Text _ s) = Text origin s
centerShape (Clipped p _ w h) = Clipped p origin w h

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter shape = centerShape $ scaleShape 2.0 shape

-- Exercise 3.3
shapeText :: Shape -> Maybe String
shapeText (Text _ s) = Just s
shapeText _ = Nothing

-- Exercise 4.1
newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)

-- Exercise 5.1
area :: Shape -> Number
area (Circle c r) = Math.pi * (Math.pow r 2.0)
area (Rectangle c w h) = w * h
area (Line s e) = 0.0
area (Text c s) = 0.0
area (Clipped p c w h) = w * h

-- Exercise 5.2
-- Solved in Picture.purs
