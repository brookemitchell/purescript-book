module Data.Brooke.Etc2 where

import Data.List
import Data.Maybe
import Data.Traversable
import Prelude

lift2 :: forall a b c f. Apply f => (a -> b -> c) -> f a -> f b -> f c
lift2 fn a b = (fn) <$> a <*> b

combineList :: forall f a. Applicative f => List (f a) -> f (List a)
combineList (Cons x xs) = Cons <$> x <*> combineList xs
combineList Nil = pure Nil

el1 :: List (Maybe Int)
el1 = (Just 1 : Nil)
combineEx :: Maybe (List Int)
combineEx = combineList el1

-- (Easy) Use lift2 to write lifted versions of the numeric operators +, -, * and / which work with optional arguments.
optPlus = lift2 (+)
optMinus = lift2 (-)
optMult = lift2 (*)
optDiv = lift2 (/)

-- (Medium) Convince yourself that the definition of lift3 given above in terms of <$> and <*> does type check.
lift3 :: forall a b c d f. Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 fn a b c = fn <$> a <*> b <*> c

-- (Difficult) Write a function combineMaybe which has type forall a f. Applicative f => Maybe (f a) -> f (Maybe a). This function takes an optional computation with side-effects, and returns a side-effecting computation which has an optional result.

combineMaybe :: forall f a. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just x) = Just <$> x
combineMaybe Nothing = pure Nothing

el2 :: Maybe (Array Int)
el2 = Just ([1,2,3])
combineEx2 :: Array (Maybe Int)
combineEx2 = combineMaybe el2

el3 :: Maybe (Array Int)
el3 = Nothing
combineEx3 :: Array (Maybe Int)
combineEx3 = combineMaybe el3

-- (Difficult) Try to write sequence in terms of traverse. Can you write traverse in terms of sequence?
sequence' :: forall a m t. Traversable t => Applicative m => t (m a) -> m (t a)
sequence' = traverse id

traverse' :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverse' f = sequence <<< map f
