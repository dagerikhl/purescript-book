module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (cons, filter, head, null, sort, tail, (..))
import Data.Foldable
import Data.Maybe (fromMaybe, Maybe)
import Data.Path
import Test.Examples

-- Note to reader: Add your solutions to this file

-- Exercise 1.1
isEven :: Int -> Boolean
--isEven x = (mod x 2) == 0
--isEven x = mod x 2 == 0
--isEven x = (x `mod` 2) == 0
isEven x = x `mod` 2 == 0

-- Exercise 1.2
countEven :: Array Int -> Int
countEven arr =
  if null arr then
    0
  else
    (if isEven (fromMaybe 0 (head arr)) then 1 else 0) + (countEven $ fromMaybe [] $ tail arr)

-- Exercise 2.1
squared :: Array Number -> Array Number
--squared arr = map (\x -> x * x) arr
squared = map (\x -> x * x)

-- Exercise 2.2
keepNonNegative :: Array Number -> Array Number
--keepNonNegative arr = filter (_ >= 0.0) arr
keepNonNegative = filter (_ >= 0.0)

-- Exercise 2.3
infix 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (_ >= 0.0) <$?> arr

-- Exercise 3.1
isPrime :: Int -> Boolean
isPrime x = (not (x == 1)) && null (fromMaybe [] (tail (factors x)))
--isPrime x = (not $ x == 1) && (null $ fromMaybe [] $ tail $ factors x)

-- Exercise 3.2
cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct a b = do
  ax <- a
  bx <- b
  [[ax, bx]]

-- Exercise 3.3
triples :: Int -> Array (Array Int)
triples n = do
  a <- 1..n
  b <- a..n
  c <- b..n
  guard $ a * a + b * b == c * c
  pure [a, b, c]

-- Exercise 3.4
infix 6 mod as %

primeFactors :: Int -> Array Int
primeFactors n = factorize 2 n
  where
  factorize :: Int -> Int -> Array Int
  factorize _ 1 = []
  factorize divisor dividend =
    if dividend % divisor == 0 then
      cons divisor $ factorize divisor (dividend / divisor)
    else
      factorize (divisor + 1) dividend

-- Exercise 4.1
allTrue :: Array Boolean -> Boolean
--allTrue arr = foldl (\acc bool -> acc && bool) true arr
allTrue arr = foldl (==) true arr

-- Exercise 4.2
-- The function `xs -> foldl (==) false xs` returns `true` when `xs` contains an odd number of `false` items.

-- Exercise 4.3
fibTailRec :: Int -> Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = fib' n 2 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2 =
    if limit == count then
      n1 + n2
    else
      fib' limit (count + 1) n2 (n1 + n2)

-- Exercise 4.4
reverse :: forall a. Array a -> Array a
--reverse arr = foldr (\x acc -> acc <> [x]) [] arr
reverse arr = foldl (\acc x -> [x] <> acc) [] arr

-- Exercise 5.1
onlyFiles :: Path -> Array Path
onlyFiles path = filter (not isDirectory) (allFiles path)

-- Exercise 5.2
whereIs :: Path -> String -> Maybe Path
whereIs path fileName = head $ do
  path' <- allFiles path
  child <- ls path'
  guard $ filename child == filename path' <> fileName
  pure path'

-- Exercise 5.3
largestSmallest :: Path -> Array Path
largestSmallest path = foldl loop [] (onlyFiles path) where
  loop :: Array Path -> Path -> Array Path
  loop [largest, smallest] current | size current < size smallest = [largest, current]
                                   | size current > size largest  = [current, smallest]
                                   | otherwise                    = [largest, smallest]
  loop [last] current              | size current < size last     = [current, last]
                                   | otherwise                    = [last, current]
  loop arr current                                                = cons current arr
