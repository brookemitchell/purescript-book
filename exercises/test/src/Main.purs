module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (random)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  n <- random
  log $ "hi " <> (show n)
