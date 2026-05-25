{-# LANGUAGE OverloadedStrings #-}

module TestSupport
  ( doc
  , docWithMeta
  , renderOne
  , renderPara
  , shouldContainT
  , shouldNotContainT
  ) where

import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Stack                 (HasCallStack)
import           Test.Hspec                (Expectation, shouldSatisfy)
import           Text.Pandoc.Definition
import           Text.Pandoc.Writers.SCDoc (writeSCDocPure)

doc :: [Block] -> Pandoc
doc = Pandoc nullMeta

docWithMeta :: [(Text, MetaValue)] -> [Block] -> Pandoc
docWithMeta kvs = Pandoc (Meta (Map.fromList kvs))

renderOne :: Block -> Text
renderOne b = T.strip $ writeSCDocPure (doc [b])

renderPara :: [Inline] -> Text
renderPara xs = T.strip $ writeSCDocPure (doc [Para xs])

shouldContainT :: HasCallStack => Text -> Text -> Expectation
shouldContainT haystack needle =
  haystack `shouldSatisfy` T.isInfixOf needle

shouldNotContainT :: HasCallStack => Text -> Text -> Expectation
shouldNotContainT haystack needle =
  haystack `shouldSatisfy` (not . T.isInfixOf needle)

infix 1 `shouldContainT`, `shouldNotContainT`
