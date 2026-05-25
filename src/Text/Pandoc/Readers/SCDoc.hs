{-# LANGUAGE OverloadedStrings #-}

-- | Pandoc reader for SuperCollider SCDoc help files.
module Text.Pandoc.Readers.SCDoc
  ( readSCDoc
  , readSCDocPure
  ) where

import           Control.Monad.Except             (throwError)
import           Data.Text                        (Text)
import qualified Data.Text                        as T

import           Text.Pandoc.Class                (PandocMonad)
import           Text.Pandoc.Definition           (Pandoc)
import           Text.Pandoc.Error                (PandocError (..))
import           Text.Pandoc.Options              (ReaderOptions)

import           Text.Pandoc.Readers.SCDoc.Lexer  (tokenize)
import           Text.Pandoc.Readers.SCDoc.Parser (parseDocument)


-- | Pandoc reader entry point for SCDoc.
readSCDoc :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readSCDoc _ = either throwError pure . readSCDocPure

-- | Pure SCDoc reader for tests and scripts.
readSCDocPure :: Text -> Either PandocError Pandoc
readSCDocPure input =
  case tokenize "<scdoc>" input of
    Left  err  -> Left (PandocParseError (T.pack (show err)))
    Right toks -> case parseDocument "<scdoc>" toks of
      Left  err -> Left (PandocParseError (T.pack (show err)))
      Right pd  -> Right pd
