-- Testing code for WHILE language
-- Add your own test cases to this file
-- Make sure that you read the .lhs version for clarification
module Tests where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad (liftM, liftM2, liftM3)
import Data.Either (isLeft)
import Data.Map (Map)
import qualified Data.Map as Map
import State (State)
import qualified State as S
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    Testable (..),
    classify,
    elements,
    frequency,
    listOf,
    maxSize,
    maxSuccess,
    oneof,
    quickCheckWith,
    resize,
    scale,
    sized,
    stdArgs,
    (==>),
  )
import Text.PrettyPrint (Doc, ($$), (<+>), (<>))
import qualified Text.PrettyPrint as PP

-----------------------------------------------------------------
-- A main action to run all the tests...

main :: IO ()
main = do
  return ()

------------------------- Test cases for the interpreter -----
