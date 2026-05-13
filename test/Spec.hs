module Main (main) where

import           Test.Hspec

import qualified PropertySpec
import qualified ReferenceSpec
-- import qualified SCDocSpec

main :: IO ()
main = hspec $ do
  -- SCDocSpec.spec
  PropertySpec.spec
  ReferenceSpec.spec
