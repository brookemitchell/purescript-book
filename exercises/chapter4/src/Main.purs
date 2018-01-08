module Main where

import Prelude
import Data.Array (null)
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)

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
