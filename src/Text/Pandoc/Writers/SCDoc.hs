{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Writers.SCDoc
  ( -- * Pandoc writer interface
    writeSCDoc
    -- * Pure entry points
  , writeSCDocPure
  , writeSCDocWithWarnings
    -- * Diagnostics
  , SCDocWarning (..)
  , formatSCDocWarning
  ) where

import           Data.Text                          (Text)
import           Text.Pandoc.Class                  (PandocMonad)
import           Text.Pandoc.Definition             (Pandoc)
import           Text.Pandoc.Options                (WriterOptions)
import           Text.Pandoc.Writers.SCDoc.Render   (render)
import           Text.Pandoc.Writers.SCDoc.Warnings (SCDocWarning (..),
                                                     collectWarnings,
                                                     formatSCDocWarning)


writeSCDoc :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeSCDoc _opts doc =
  pure (render doc)

writeSCDocPure :: Pandoc -> Text
writeSCDocPure = render

writeSCDocWithWarnings :: Pandoc -> (Text, [SCDocWarning])
writeSCDocWithWarnings doc =
  (render doc, collectWarnings doc)
