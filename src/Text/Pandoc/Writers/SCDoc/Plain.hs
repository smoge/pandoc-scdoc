{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Text.Pandoc.Writers.SCDoc.Plain
  ( plainTextInlines
  ) where

import           Data.Text              (Text)
import qualified Data.Text              as T

import           Text.Pandoc.Definition

plainTextInlines :: [Inline] -> Text
plainTextInlines = oneLine . T.concat . map plainTextInline

plainTextInline :: Inline -> Text
plainTextInline = \case
  Str s                   -> s
  Space                   -> " "
  SoftBreak               -> " "
  LineBreak               -> " "
  Emph xs                 -> plainTextInlines xs
  Strong xs               -> plainTextInlines xs
  Underline xs            -> plainTextInlines xs
  Strikeout xs            -> plainTextInlines xs
  Superscript xs          -> plainTextInlines xs
  Subscript xs            -> plainTextInlines xs
  SmallCaps xs            -> plainTextInlines xs
  Quoted SingleQuote xs   -> "'"  <> plainTextInlines xs <> "'"
  Quoted DoubleQuote xs   -> "\"" <> plainTextInlines xs <> "\""
  Cite _ xs               -> plainTextInlines xs
  Code _ s                -> s
  Math _ s                -> s
  RawInline _ s           -> s
  Link _ label _          -> plainTextInlines label
  Image _ alt _           -> plainTextInlines alt
  Note _                  -> ""
  Span _ xs               -> plainTextInlines xs

oneLine :: Text -> Text
oneLine = T.unwords . T.words
