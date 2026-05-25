{-# LANGUAGE OverloadedStrings #-}

module ReferenceSpec (spec) where

import           Control.Monad             (forM)
import           Data.List                 (sort)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           System.Directory          (listDirectory)
import           System.FilePath           (replaceExtension, takeExtension,
                                            (</>))
import           Test.Hspec

import           Control.Monad.Error.Class (throwError)
import qualified Text.Pandoc               as Pandoc
import           Text.Pandoc.Error         (PandocError (..))
import           Text.Pandoc.Options       (ReaderOptions (..),
                                            pandocExtensions)

import           Text.Pandoc.Readers.SCDoc (readSCDoc)
import           Text.Pandoc.Writers.SCDoc (writeSCDocPure)

referenceDir :: FilePath
referenceDir = "test/reference"

mdReaderOpts :: ReaderOptions
mdReaderOpts = Pandoc.def { readerExtensions = pandocExtensions }

renderDoc :: FilePath -> IO T.Text
renderDoc path = do
  input  <- TIO.readFile path
  result <- Pandoc.runIO $ do
    doc <- case takeExtension path of
      ".md"   -> Pandoc.readMarkdown mdReaderOpts input
      ".rst"  -> Pandoc.readRST      Pandoc.def   input
      ".html" -> Pandoc.readHtml     Pandoc.def   input
      ".org"  -> Pandoc.readOrg      Pandoc.def   input
      ".schelp" -> readSCDoc         Pandoc.def   input
      ext     -> throwError (PandocAppError (T.pack ("renderDoc: unsupported extension " <> ext)))
    pure (writeSCDocPure doc)
  case result of
    Left  err -> fail (show err)
    Right txt -> pure txt

renderSourceThroughSCDocReader :: FilePath -> IO T.Text
renderSourceThroughSCDocReader path = do
  input <- renderDoc path
  result <- Pandoc.runIO $ do
    doc <- readSCDoc Pandoc.def input
    pure (writeSCDocPure doc)
  case result of
    Left  err -> fail (show err)
    Right txt -> pure txt

spec :: Spec
spec = do
  describe "reference files" $ do
    pairs <- runIO $ do
      let sourceExts = [".md", ".rst", ".html", ".org"]
      files <- sort
             . filter ((`elem` sourceExts) . takeExtension)
             <$> listDirectory referenceDir
      forM files $ \srcFile ->
        return (referenceDir </> srcFile, referenceDir </> replaceExtension srcFile ".schelp")

    mapM_ referenceTest pairs

  describe "SCDoc reader reference files" $ do
    schelpFiles <- runIO $ do
      files <- sort
             . filter ((== ".schelp") . takeExtension)
             <$> listDirectory referenceDir
      pure (fmap (referenceDir </>) files)

    mapM_ scdocRoundTripTest schelpFiles

  describe "Markdown to SCDoc reader roundtrip reference files" $ do
    pairs <- runIO $ do
      files <- sort
             . filter ((== ".md") . takeExtension)
             <$> listDirectory referenceDir
      forM files $ \srcFile ->
        return (referenceDir </> srcFile, referenceDir </> replaceExtension srcFile ".schelp")

    mapM_ markdownSCDocRoundTripTest pairs


referenceTest :: (FilePath, FilePath) -> Spec
referenceTest (srcPath, referencePath) =
  it srcPath $ do
    actual   <- renderDoc srcPath
    expected <- TIO.readFile referencePath
    actual `shouldBe` expected

scdocRoundTripTest :: FilePath -> Spec
scdocRoundTripTest path =
  it path $ do
    actual   <- renderDoc path
    expected <- TIO.readFile path
    actual `shouldBe` expected

markdownSCDocRoundTripTest :: (FilePath, FilePath) -> Spec
markdownSCDocRoundTripTest (srcPath, referencePath) =
  it srcPath $ do
    actual   <- renderSourceThroughSCDocReader srcPath
    expected <- TIO.readFile referencePath
    actual `shouldBe` expected
