module Stream where


import Data.Array as Array
import Data.Maybe
import Data.String as String
import Prelude
import Data.Monoid
import Data.Array.Partial as Partial


class Stream stream element | stream -> element where
  uncons :: stream -> Maybe { head :: element , tail :: stream }


instance streamArray :: Stream (Array a) a where
  uncons = Array.uncons


instance streamString :: Stream String Char where
  uncons = String.uncons


foldStream :: forall l e m. Stream l e => Monoid m => (e -> m) -> l -> m
foldStream f list =
  case uncons list of
    Nothing -> mempty
    Just cons -> f cons.head <> foldStream f cons.tail

-- genericTail xs = map _.tail (uncons xs)
-- x = map _.tail (uncons "testing")

secondElement :: forall a. Partial => Array a -> a
secondElement xs = Partial.head (Partial.tail xs)
