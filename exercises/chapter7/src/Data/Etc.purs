module Data.Brooke.Etc where

import Data.List
import Data.Maybe
import Data.Traversable
import Prelude

import Data.Monoid (mempty)

-- 1. (Medium) Write a Traversable instance for the following binary tree data structure, which combines side-effects from left-to-right:

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance showTree :: (Show a) => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch x y z) = "(Branch " <> show x <> " " <> show y <> " " <> show z <> ")"

instance functorTree :: Functor Tree where
  map _ (Leaf) = Leaf
  map f (Branch left val right) = Branch (map f left) (f val) (map f right)


instance foldableTree :: Foldable (Tree) where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap _ Leaf = mempty
  foldMap f (Branch left a right) = foldMap f left <> (f a) <> foldMap f right

instance traverseTree :: Traversable Tree where
  traverse f Leaf = pure Leaf
  traverse f (Branch l x r) = Branch <$> traverse f l <*> f x <*> traverse f r
  sequence = traverse id

instance eqTree :: Show a => Eq (Tree a) where
  eq t1 t2 = let stringify = foldMap show in
              stringify t1 == stringify t2

-- This corresponds to an in-order traversal of the tree. What about a preorder traversal? What about reverse order?

add1 = (+) 1
t1 = Branch (Leaf) 1 Leaf
t2 = Branch (Branch Leaf 2 Leaf) 1 Leaf
t3 = Branch (Branch (Branch Leaf 3 Leaf) 2 Leaf) 1 Leaf
t10 = Branch (Branch Leaf "1" Leaf) "2." Leaf
t11 = Branch (Branch (Branch Leaf "3" Leaf) "2" Leaf) "1" Leaf

ex1 = map add1 t2
ex2 = foldr (+) 0 t2
ex3 = foldr (+) 0 t3
ex10 = foldMap id t10
ex11 = foldMap id t11

ex4 = traverse (\x -> [x]) t3

sup = Branch Leaf [1, 2] (Branch Leaf [3,4] Leaf)
ex5 = sequence sup

-- (Medium) Modify the code to make the address field of the Person type optional using Data.Maybe. Hint: Use traverse to validate a field of type Maybe a.
