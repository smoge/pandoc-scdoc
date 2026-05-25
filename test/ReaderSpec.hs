{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the default and pure SCDoc reader entry points.
module ReaderSpec (spec) where

import           Data.Either               (isLeft)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Test.Hspec

import qualified Text.Pandoc               as Pandoc
import           Text.Pandoc.Definition
import           Text.Pandoc.Options       (def)
import           Text.Pandoc.Readers.SCDoc (readSCDoc, readSCDocPure)


readDefault :: Text -> IO Pandoc
readDefault = runOrFail . readSCDoc def

runOrFail :: Pandoc.PandocIO Pandoc -> IO Pandoc
runOrFail action = do
  r <- Pandoc.runIO action
  case r of
    Left  e  -> error (show e)
    Right pd -> pure pd


-- | Fixture covering the reader's normalized AST shapes.
fixture :: Text
fixture = T.unlines
  [ "TITLE:: Example"
  , "summary:: A small fixture for the default-reader test"
  , ""
  , "DESCRIPTION::"
  , "Short prose."
  , ""
  , "CLASSMETHODS::"
  , ""
  , "method:: new"
  , "Construct one."
  , ""
  , "INSTANCEMETHODS::"
  , ""
  , "private:: helper, info_"
  , ""
  , "method:: play"
  , "Run it."
  , ""
  , "returns:: a UGen"
  , ""
  , "EXAMPLES::"
  , ""
  , "definitionlist::"
  , "## one"
  , "## two"
  , "|| shared body"
  , "::"
  ]


spec :: Spec
spec =
  describe "SCDoc reader entry points" $ do

    it "readSCDoc and readSCDocPure produce the same AST on the fixture" $ do
      d <- readDefault fixture
      case readSCDocPure fixture of
        Left  err -> error ("readSCDocPure unexpectedly failed: " <> show err)
        Right pd  -> pd `shouldBe` d

    -- Malformed input should surface as 'Left'.
    it "readSCDocPure surfaces parse failures as Left" $
      readSCDocPure (T.pack "teletype:: unterminated\n")
        `shouldSatisfy` isLeft
