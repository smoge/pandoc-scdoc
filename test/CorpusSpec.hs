{-# LANGUAGE OverloadedStrings #-}

-- | Corpus smoke tests against a local SuperCollider @HelpSource@ checkout.
-- Enabled when @SCDOC_HELPSOURCE@ points at a checkout.
module CorpusSpec (spec) where

import           Control.Exception         (evaluate)
import           Control.Monad             (forM)
import           Data.List                 (sort)
import qualified Data.Text.IO              as TIO
import           System.Directory          (doesDirectoryExist,
                                            listDirectory)
import           System.Environment        (lookupEnv)
import           System.FilePath           (takeExtension, takeFileName,
                                            (</>))
import           Test.Hspec

import qualified Text.Pandoc               as Pandoc
import           Text.Pandoc.Options       (def)
import           Text.Pandoc.Readers.SCDoc (readSCDoc)
import           Text.Pandoc.Writers.SCDoc (writeSCDoc)


helpSourceEnv :: String
helpSourceEnv = "SCDOC_HELPSOURCE"


helpSourceRootFromEnv :: IO (Maybe FilePath)
helpSourceRootFromEnv = do
  root <- lookupEnv helpSourceEnv
  pure $ case root of
    Just path | not (null path) -> Just path
    _                           -> Nothing


-- | Known parser failures. Keep empty unless upstream has a documented quirk.
knownFailures :: [FilePath]
knownFailures = []


-- | Recursively list every @.schelp@ file under a directory.
findSchelps :: FilePath -> IO [FilePath]
findSchelps root = do
  entries <- listDirectory root
  fmap concat . forM entries $ \e -> do
    let p = root </> e
    isDir <- doesDirectoryExist p
    if isDir
      then findSchelps p
      else if takeExtension p == ".schelp"
        then pure [p]
        else pure []


spec :: Spec
spec = do
  mCorpusRoot <- runIO helpSourceRootFromEnv
  corpusExists <- runIO $
    maybe (pure False) doesDirectoryExist mCorpusRoot

  case mCorpusRoot of
    Nothing -> pure ()
    Just corpusRoot | not corpusExists ->
      describe "Reader on the HelpSource corpus" $
        it "requires an existing HelpSource checkout" $
          expectationFailure ("corpus root not found at " <> corpusRoot)
    Just corpusRoot -> do
      describe "Reader on the HelpSource corpus" $ do

        files <- runIO (sort <$> findSchelps corpusRoot)

        it "the corpus root has at least 100 .schelp files (sanity)" $
          length files `shouldSatisfy` (>= 100)

        let expectedFailures = knownFailures

        it "parses every .schelp file except the known-quirky ones" $ do
          results <- forM files $ \p -> do
            input <- TIO.readFile p
            case Pandoc.runPure (readSCDoc def input) of
              Left  _ -> pure (p, False)
              Right _ -> pure (p, True)
          let actualFailures = [p | (p, False) <- results]
              unexpectedFails =
                filter (`notElem` expectedFailures) actualFailures
              missingFails =
                filter (`notElem` actualFailures) expectedFailures
          unexpectedFails `shouldBe` []
          missingFails    `shouldBe` []

        it "rejects only known-quirky inputs (forces evaluation)" $ do
          _ <- evaluate (length files)
          pure ()

      describe "Reader/writer/reader round-trip on selected corpus files" $
        mapM_ roundTripFile
          [ corpusRoot </> "Classes/SinOsc.schelp"
              -- canonical class help: class::, method args, related::
          , corpusRoot </> "Classes/Environment.schelp"
              -- method:: name (args)
          , corpusRoot </> "Guides/Glossary.schelp"
              -- definitionlist keyword:: anchors
          , corpusRoot </> "Reference/EmacsEditor.schelp"
              -- multi-line definitionlist terms
          , corpusRoot </> "Classes/AbstractFunction.schelp"
              -- subsection:: inside an instanceMethods:: section
          , corpusRoot </> "Classes/MIDIIn.schelp"
              -- table cell with nested list
          , corpusRoot </> "Classes/SynthDef.schelp"
              -- multi-line definition body
          , corpusRoot </> "Reference/Resize.schelp"
              -- explicit section:: titles
          , corpusRoot </> "Reference/Classes.schelp"
              -- explicit methods-section titles
          , corpusRoot </> "Classes/HID.schelp"
              -- private setter names and multi-block def bodies
          , corpusRoot </> "Classes/NodeProxy.schelp"
              -- copymethod:: label with class plus comma-list methods
          , corpusRoot </> "Reference/SCDocSyntax.schelp"
              -- syntax examples for escaping and definition lists
          , corpusRoot </> "Classes/Date.schelp"
              -- redundant link labels
          , corpusRoot </> "Classes/Clock.schelp"
              -- links with .html suffixes
          , corpusRoot </> "Guides/GUI-Introduction.schelp"
              -- malformed anchored link with trailing #
          , corpusRoot </> "Classes/SendReply.schelp"
              -- nested list inside a list item
          , corpusRoot </> "Classes/Exception.schelp"
              -- body-position keyword:: metadata
          , corpusRoot </> "Reference/Syntax-Shortcuts.schelp"
              -- table cell containing a CodeBlock
          , corpusRoot </> "Reference/Server-Command-Reference.schelp"
              -- table cell containing a nested Table
          , corpusRoot </> "Classes/LevelIndicator.schelp"
              -- method:: warning must not become warning::
          ]


-- | Reader/writer/reader AST equality for one corpus file.
roundTripFile :: FilePath -> Spec
roundTripFile path = it ("round-trips " <> takeFileName path) $ do
  src    <- TIO.readFile path
  result <- Pandoc.runIO $ do
    ast1    <- readSCDoc def src
    written <- writeSCDoc def ast1
    ast2    <- readSCDoc def written
    pure (ast1, ast2)
  case result of
    Left  e            -> expectationFailure (show e)
    Right (ast1, ast2) -> ast2 `shouldBe` ast1
