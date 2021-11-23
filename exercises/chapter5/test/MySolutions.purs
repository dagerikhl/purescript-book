module Test.MySolutions where

import Prelude

import Data.Person (Person)

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
