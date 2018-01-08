module IntOps where

import Prelude
import Data.Array (null, filter, (..), length)
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)

-- (Easy) Write a recursive function which returns true if and only if its input is an even integer.
isEvenInt :: Int -> Boolean
isEvenInt a = a `mod` 2 == 0

-- (Medium) Write a recursive function which counts the number of even integers in an array. Hint: the function unsafePartial head (where head is also imported from Data.Array.Partial) can be used to find the first element in a non-empty array.
numberOfEvens :: Array Int -> Int
numberOfEvens arr =
  if null arr
    then 0
    else (isEven $ (unsafePartial head arr)) + (numberOfEvens $ (unsafePartial tail arr))
  where
    isEven :: Int -> Int
    isEven i =
      if i `mod` 2 == 0 then 1 else 0

  -- (Easy) Use the map or <$> function to write a function which calculates the squares of an array of numbers.
squareArr :: Array Int -> Array Int
squareArr = (<$>) \n -> n * n

-- (Easy) Use the filter function to write a function which removes the negative numbers from an array of numbers.
filterNeg :: Array Int -> Array Int
filterNeg i = (\n -> n > -1) <$?> i

-- (Medium) Define an infix synonym <$?> for filter. Rewrite your answer to the previous question to use your new operator. Experiment with the precedence level and associativity of your operator in PSCi.
infixl 7 filter as <$?>

-- (Easy) Use the factors function to define a function isPrime which tests if its integer argument is prime or not.

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime = (==) 1 <<< length <<< factors

-- (Medium) Write a function which uses do notation to find the cartesian product of two arrays, i.e. the set of all pairs of elements a, b, where a is an element of the first array, and b is an element of the second.

cartesianProd :: Array Int -> Array Int -> Array (Array Int)
cartesianProd a1 a2 = do
  a <- a1
  b <- a2
  pure [a, b]

-- (Medium) A Pythagorean triple is an array of numbers [a, b, c] such that a² + b² = c². Use the guard function in an array comprehension to write a function triples which takes a number n and calculates all Pythagorean triples whose components are less than n. Your function should have type Int -> Array (Array Int).
triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b ..n
  guard $ a * a + b * b == c * c
  pure [a, b, c]
