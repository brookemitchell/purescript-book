module Data.Etc where

import Partial.Unsafe
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Array as Array
import Data.Hashable (hash, hashEqual)
import Prelude

-- Write an Eq instance for the type NonEmpty a which reuses the instances for Eq a and Eq (Array a).
data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty a arr) (NonEmpty b brr) = a == b

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty a arr) = show ([a] <> arr )

-- (Medium) Write a Semigroup instance for NonEmpty a by reusing the Semigroup instance for Array.
instance semigroupNonEmpty :: Semigroup a => Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x  (xs <> [y] <> ys)

-- (Medium) Write a Functor instance for NonEmpty.
instance functorNonEmpty :: Functor NonEmpty  where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

-- (Medium) Given any type a with an instance of Ord, we can add a new “infinite” value which is greater than any other value:
-- Write an Ord instance for Extended a which reuses the Ord instance for a.

data Extended a = Finite a | Infinite

instance showExtended :: (Show a) => Show (Extended a) where
  show (Infinite _) = "Infinite"
  show (Finite x) = "(Finite " <> show x <> ")"

instance eqExtended :: Eq (Extended a) where
  eq _ _ = true

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite (Finite y) = GT
  compare (Finite x) Infinite = LT
  compare (Finite x) (Finite y) = compare x y

-- (Difficult) Write a Foldable instance for NonEmpty. Hint: reuse the Foldable instance for arrays.
instance foldNonEmpty :: Foldable NonEmpty where
  foldl f acc (NonEmpty x xs) = Array.foldl f acc ([x] <> xs)
  foldr f acc (NonEmpty x xs) = Array.foldr f acc ([x] <> xs)
  foldMap f (NonEmpty x xs) = Array.foldMap f ([x] <> xs)

-- (Difficult) Given an type constructor f which defines an ordered container (and so has a Foldable instance), we can create a new container type which includes an extra element at the front:
data OneMore f a = OneMore a (f a)

-- The container OneMore f is also has an ordering, where the new element comes before any element of f. Write a Foldable instance for OneMore f:
instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f z (OneMore x ys) = f x (foldr f z ys)
  foldl f z (OneMore x ys) = f (foldl f z ys) x
  foldMap f (OneMore x ys) = (f x) <> (foldMap f ys)

-- (Medium) Define a partial function which finds the maximum of a non-empty array of integers. Your function should have type Partial => Array Int -> Int. Test out your function in PSCi using unsafePartial. Hint: Use the maximum function from Data.Foldable.
maxEl :: Partial => Array Int -> Int
maxEl =  fromJust <<< maximum

-- (Medium) The Action class is a multi-parameter type class which defines an action of one type on another:

class Monoid m <= Action m a where
  act :: m -> a -> a

-- An action is a function which describes how monoidal values can be used to modify a value of another type. There are two laws for the Action type class:

-- act mempty a = a
-- act (append m1 m2) a = act m1 (act m2 a)

-- That is, the action respects the operations defined by the Monoid class. For example, the natural numbers form a monoid under multiplication:

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

-- This monoid acts on strings by repeating an input string some number of times. Write an instance which implements this action:
-- Does this instance satisfy the laws listed above?

-- instance repeatAction :: Action Multiply String where
--   act mon actF  =

-- act mempty a = a
-- act (append m1 m2) a = act m1 (act m2 a)
