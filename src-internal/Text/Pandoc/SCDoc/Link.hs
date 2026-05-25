{-# LANGUAGE OverloadedStrings #-}

-- | Shared link and image canonicalization for the SCDoc reader/writer.
module Text.Pandoc.SCDoc.Link
  ( normalizeLinkTarget
  , normalizeImageTarget
  , defaultLinkLabel
  , shouldOmitLinkLabel
  , linkHasAnchor
  , isExternalUrl
  , stripKnownExt
  , dropAnyPrefix
  ) where

import           Data.Text (Text)
import qualified Data.Text as T


-- | Canonicalize an internal SCDoc link target.
normalizeLinkTarget :: Text -> Text
normalizeLinkTarget target
  | isExternalUrl target = target
  | otherwise            = stripKnownExt path <> frag
  where
    (path0, frag) = T.breakOn "#" target
    path          = dropAnyPrefix ["./HelpSource/", "HelpSource/", "./"] path0


-- | Canonicalize an internal image target without stripping extensions.
normalizeImageTarget :: Text -> Text
normalizeImageTarget =
  dropAnyPrefix ["./HelpSource/", "HelpSource/", "./"]


-- | Default visible label for a target: final path segment, no fragment.
defaultLinkLabel :: Text -> Text
defaultLinkLabel target =
  case filter (not . T.null) (T.splitOn "/" (T.takeWhile (/= '#') target)) of
    [] -> ""
    xs -> last xs


-- | True when the rendered label would be the same without an explicit label.
shouldOmitLinkLabel :: Text -> Text -> Bool
shouldOmitLinkLabel target label =
  T.null label
  || label == target
  || (not (linkHasAnchor target) && label == defaultLinkLabel target)


-- | True when the target encodes an internal anchor (single @#@
-- separator, not the @##@ that introduces a label).
linkHasAnchor :: Text -> Bool
linkHasAnchor target =
  not (isExternalUrl target)
  && "#" `T.isInfixOf` target
  && not ("##" `T.isInfixOf` target)


-- | True for URLs that should be treated as opaque (no normalization).
isExternalUrl :: Text -> Bool
isExternalUrl x =
  any (`T.isPrefixOf` T.toLower x)
    ["http://", "https://", "ftp://", "mailto:", "file://"]


-- | Drop one known document suffix, case-insensitively.
stripKnownExt :: Text -> Text
stripKnownExt s = foldr stripOne s [".schelp", ".md", ".html"]
  where
    stripOne ext x
      | ext `T.isSuffixOf` T.toLower x = T.dropEnd (T.length ext) x
      | otherwise                      = x


-- | Strip matching prefixes repeatedly.
dropAnyPrefix :: [Text] -> Text -> Text
dropAnyPrefix prefixes s =
  case [T.drop (T.length p) s | p <- prefixes, p `T.isPrefixOf` s] of
    (x:_) -> dropAnyPrefix prefixes x
    []    -> s
