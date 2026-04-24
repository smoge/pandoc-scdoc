{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO              as TIO
import           System.Environment        (getArgs)

import           Text.Pandoc               (def, readMarkdown, runIO)
import           Text.Pandoc.Error         (handleError)
import           Text.Pandoc.Options       (ReaderOptions (..), WriterOptions,
                                            pandocExtensions)

import           Text.Pandoc.Writers.SCDoc (writeSCDoc)

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    []  -> TIO.getContents
    [p] -> TIO.readFile p
    _   -> fail "Usage: md2schelp file.md > file.schelp"

  result <- runIO $ do
    doc <- readMarkdown readerOpts input
    writeSCDoc writerOpts doc

  output <- handleError result

  TIO.putStr output


readerOpts :: ReaderOptions
readerOpts = def { readerExtensions = pandocExtensions }

writerOpts :: WriterOptions
writerOpts = def
