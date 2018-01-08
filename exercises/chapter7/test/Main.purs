module Test.Main where

import Prelude

import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.AddressBook (examplePerson)
import Data.AddressBook.Validation (validatePerson)

import Control.Monad.Eff (Eff)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Data.Brooke.Etc

main :: Eff (RunnerEffects (console :: CONSOLE)) Unit
main = do
  -- logShow (validatePerson examplePerson)
  run [consoleReporter] do
    describe "Features" do
      let add1 = (+) 1
      let t1 = Branch (Leaf) 1 Leaf
      let t2 = Branch (Branch Leaf 2 Leaf) 1 Leaf
      let t3 = Branch (Branch (Branch Leaf 3 Leaf) 2 Leaf) 1 Leaf
      let t10 = Branch (Branch Leaf "1" Leaf) "2." Leaf
      let t11 = Branch (Branch (Branch Leaf "3" Leaf) "2" Leaf) "1" Leaf
      it "map tree" $ do
        let ex1 = map add1 t2
        let res1 = Branch (Branch Leaf 3 Leaf) 2 Leaf
        ex1 `shouldEqual` res1
        let ex2 = map ((<>) "!") t10
        let res2 = Branch (Branch Leaf "!1" Leaf) "!2." Leaf
        ex2 `shouldEqual` res2


-- This corresponds to an in-order traversal of the tree. What about a preorder traversal? What about reverse order?

-- add1 = (+) 1

-- ex1 = map add1 t2
-- ex2 = foldr (+) 0 t2
-- ex3 = foldr (+) 0 t3
-- ex10 = foldMap id t10
-- ex11 = foldMap id t11
-- ex4 = traverse Just t3

-- sup = Branch Leaf [1, 2] (Branch Leaf [3,4] Leaf)
-- ex5 = sequence sup

-- (Medium) Modify the code to make the address field of the Person type optional using Data.Maybe. Hint: Use traverse to validate a field of type Maybe a.
