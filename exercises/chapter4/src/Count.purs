module Count where

import Prelude

import Partial.Unsafe (unsafePartial)
import Data.Array.Partial (head, tail)
import Data.Array ((:))
import Data.Foldable (foldl)


-- (Easy) Use foldl to test whether an array of boolean values are all true.

-- (Medium) Characterize those arrays xs for which the function foldl (==) false xs returns true.
  -- either odd and all the same, or, even and one different

-- (Medium) Rewrite the following function in tail recursive form using an accumulator parameter:
--  import Prelude

count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = if p (unsafePartial head xs)
               then count p (unsafePartial tail xs) + 1
               else count p (unsafePartial tail xs)


count' :: forall a. (a -> Boolean) -> Array a -> Int
count' p xs = count'' 0 p xs
  where
    count'' acc _ [] = acc
    count'' acc f ys = count''(
      (+) acc $
      if p $
         unsafePartial head ys
      then 1
      else 0) f $
    unsafePartial tail ys

-- (Medium) Write reverse in terms of foldl.
reverse :: forall a. Array a -> Array a
reverse = foldl (\acc e -> [e] <> acc) []
