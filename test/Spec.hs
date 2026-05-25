module Main (main) where

import           Test.Hspec

import qualified CorpusAuditSpec
import qualified CorpusSpec
import qualified LexerSpec
import qualified ParserSpec
import qualified PropertySpec
import qualified ReaderSpec
import qualified ReferenceSpec
import qualified WriterMetadataSpec
import qualified WriterSpec

main :: IO ()
main = hspec $ do
  WriterSpec.spec
  WriterMetadataSpec.spec
  LexerSpec.spec
  ParserSpec.spec
  PropertySpec.spec
  ReaderSpec.spec
  ReferenceSpec.spec
  CorpusSpec.spec
  CorpusAuditSpec.spec
