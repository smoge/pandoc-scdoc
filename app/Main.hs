{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           System.Environment        (getArgs)

import           Control.Monad.Error.Class (throwError)
import           Text.Pandoc               (Reader (..), def, getReader, runIO)
import           Text.Pandoc.Error         (PandocError (..), handleError)
import           Text.Pandoc.Extensions    (Extension (Ext_smart), disableExtension)
import           Text.Pandoc.Format        (parseFlavoredFormat)
import           Text.Pandoc.Options       (ReaderOptions (..))
import           Text.Pandoc.Readers.SCDoc (readSCDoc)
import           Text.Pandoc.Writers.SCDoc (writeSCDoc)

main :: IO ()
main = do
  (fmt, mpath) <- parseArgs
  input <- maybe TIO.getContents TIO.readFile mpath
  result <- runIO $ do
    doc <- if isSCDocFormat fmt
      then readSCDoc def input
      else do
        flavored        <- parseFlavoredFormat fmt
        (reader, exts) <- getReader flavored
        let ropts = def { readerExtensions = disableExtension Ext_smart exts }
        case reader of
          TextReader       r -> r ropts input
          ByteStringReader _ -> throwError (PandocAppError "binary input formats are not supported")
    writeSCDoc def doc
  TIO.putStr =<< handleError result

isSCDocFormat :: T.Text -> Bool
isSCDocFormat fmt = T.toLower fmt `elem` ["schelp", "scdoc"]

parseArgs :: IO (T.Text, Maybe FilePath)
parseArgs = getArgs >>= \case
  [flag, fmt]       | flag `elem` ["-f", "--from"] -> pure (T.pack fmt, Nothing)
  [flag, fmt, path] | flag `elem` ["-f", "--from"] -> pure (T.pack fmt, Just path)
  [path]            -> pure ("markdown", Just path)
  []                -> pure ("markdown", Nothing)
  _                 -> fail "Usage: pandoc-scdoc [-f FORMAT] [FILE]"
