module Data.Etc where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throw, throwException)
import Data.Array (foldM, head, tail, nub, sort)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

-- (Easy) Look up the types of the head and tail functions from the Data.Array module in the purescript-arrays package. Use do notation with the Maybe monad to combine these functions into a function third which returns the third element of an array with three or more elements. Your function should return an appropriate Maybe type.

third :: forall a. Array a -> Maybe a
third arr = do
  noFirst <- tail arr
  noSecond <- tail noFirst
  third <- head noSecond
  pure third

-- (Medium) Write a function sums which uses foldM to determine all possible totals that could be made using a set of coins. The coins will be specified as an array which contains the value of each coin. Your function should have the following result:
--  > sums []
--  [0]

--  > sums [1, 2, 10]
--  [0,1,2,3,10,11,12,13]

-- Hint: This function can be written as a one-liner using foldM. You might want to use the nub and sort functions to remove duplicates and sort the result respectively.
sums :: Array Int -> Array Int
sums arr = foldM (\x y -> [x, x + y]) 0 arr # nub # sort

-- (Medium) Confirm that the ap function and the apply operator agree for the Maybe monad.

applyMaybe :: Maybe Int
applyMaybe = (+) <$> Just 1 <*> Just 2

apMaybe :: Maybe Int
apMaybe = ((+) <$> Just 1) `ap` Just 2

-- (Medium) Verify that the monad laws hold for the Monad instance for the Maybe type, as defined in the purescript-maybe package.

-- The right-identity law is the simplest of the three laws. It tells us that we can eliminate a call to pure if it is the last expression in a do notation block:
expr :: Maybe Int
expr = pure 1
maybeRight :: Maybe Int
maybeRight = do
  x <- expr
  pure x

-- The right-identity law says that this is equivalent to just expr.
-- maybeRight == expr

-- The left-identity law states that we can eliminate a call to pure if it is the first expression in a do notation block:

f :: Int -> Maybe Int
f = \x -> Just (x + 1)

leftId :: Maybe Int
leftId = do
  x <- pure 1
  f x -- next

-- leftId == f 1
-- This code is equivalent to next, after the name x has been replaced with the expression Just 1.

-- The last law is the associativity law. It tells us how to deal with nested do notation blocks. It states that the following piece of code:

-- TODO
-- c1 = do
--   y <- do
--     x <- m1
--     m2
--   m3

-- is equivalent to this code:

-- c2 = do
--   x <- m1
--   y <- m2
--   m3

-- Each of these computations involves three monadic expression m1, m2 and m3. In each case, the result of m1 is eventually bound to the name x, and the result of m2 is bound to the name y.

test :: Maybe Int
test = do
  x <- pure 1
  (\y -> Just (y + 1)) x

-- todo: other 8.7 exercises
-- 5. (Medium) Write a function filterM which generalizes the filter function on lists. Your function should have the following type signature:
-- Test your function in PSCi using the Maybe and Array monads.
filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM f Nil = pure Nil
filterM f (Cons x xs)  = do
  isGood <- f x
  xs' <- filterM f xs
  pure if isGood then (x : xs') else xs'


t1 :: List Int
t1 = (1 : 2 : 3 : Nil)

ex99 = filterM (\x -> Just (x < 3)) t1

-- 6. (Difficult) Every monad has a default Functor instance given by:
--  map f a = do
--    x <- a
--    pure (f x)
-- Use the monad laws to prove that for any monad, the following holds:

--  lift2 f (pure a) (pure b) = pure (f a b)
-- where the Applicative instance uses the ap function defined above. Recall that lift2 was defined as follows:

--  lift2 :: forall f a b c. Applicative f => (a -> b -> c) -> f a -> f b -> f c
--  lift2 f a b = f <$> a <*> b

-- TODO:
-- Monad laws:
-- Left Identity
-- Right Identity
-- Associativity

-- 8.17
-- (Medium) Rewrite the safeDivide function to throw an exception using throwException if the denominator is zero.


safeDivide :: forall eff. Int -> Int -> Eff (exception :: EXCEPTION | eff) Int
safeDivide x 0 = throwException $ error "Denominator must not be zero"
safeDivide x y = pure $ x / y

