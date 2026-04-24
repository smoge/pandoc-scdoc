{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Text.Pandoc.Writers.SCDoc.Warnings
  ( SCDocWarning (..)
  , formatSCDocWarning
  , collectWarnings
  ) where

import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Text.Pandoc.Definition
import           Text.Pandoc.Writers.SCDoc.Render (hasClass, isRawSCDocFormat)

data SCDocWarning = SCDocWarning
  { warningKind    :: Text
  , warningMessage :: Text
  } deriving (Eq, Show)

formatSCDocWarning :: SCDocWarning -> Text
formatSCDocWarning (SCDocWarning k msg) = "[" <> k <> "] " <> msg

warn :: Text -> Text -> SCDocWarning
warn = SCDocWarning

-- runs all checks
collectWarnings :: Pandoc -> [SCDocWarning]
collectWarnings (Pandoc _meta blocks) =
  concatMap checkBlock blocks
  <> checkMethodStructure blocks


checkBlock :: Block -> [SCDocWarning]
checkBlock = \case
  Plain xs        -> concatMap checkInline xs
  Para xs         -> concatMap checkInline xs
  LineBlock ls    -> concatMap (concatMap checkInline) ls
  BlockQuote bs   -> concatMap checkBlock bs
  BulletList xs   -> concatMap (concatMap checkBlock) xs
  OrderedList _ xs -> concatMap (concatMap checkBlock) xs

  DefinitionList defs ->
    concat [ concatMap checkInline t <> concatMap (concatMap checkBlock) ds
           | (t, ds) <- defs ]

  Header _ (_, classes, _) xs ->
    unknownHeaderClass classes <> concatMap checkInline xs

  Table _ _ _ (TableHead _ hr) bodies (TableFoot _ fr) ->
    concatMap checkRow (hr <> concatMap bodyRows bodies <> fr)
    where
      bodyRows (TableBody _ _ h r) = h <> r
      checkRow (Row _ cells) = concatMap checkCell cells
      checkCell (Cell _ _ _ _ bs) =
        [ warn "table-cell" "Table cell contains '||', which is the SCDoc cell separator."
        | "||" `T.isInfixOf` renderRough bs
        ]
        <> concatMap checkBlock bs

  Figure _ (Caption _ cap) bs ->
    concatMap checkBlock bs <> concatMap checkBlock cap

  Div _ bs -> concatMap checkBlock bs

  RawBlock fmt _
    | isRawSCDocFormat fmt -> []
    | otherwise -> [warn "raw-block" ("UNSUPPORTED raw block: " <> formatName fmt)]

  _ -> []

checkInline :: Inline -> [SCDocWarning]
checkInline = \case
  Emph xs      -> modalFlattening "emphasis" xs  <> concatMap checkInline xs
  Strong xs    -> modalFlattening "strong"   xs  <> concatMap checkInline xs
  Strikeout xs -> modalFlattening "soft"     xs  <> concatMap checkInline xs
  Link _ lbl _ -> modalFlattening "link"     lbl <> concatMap checkInline lbl

  Note bs      -> concatMap checkBlock bs
  Span _ xs    -> concatMap checkInline xs
  Quoted _ xs  -> concatMap checkInline xs
  Cite _ xs    -> concatMap checkInline xs

  RawInline fmt _
    | isRawSCDocFormat fmt -> []
    | otherwise -> [warn "raw-inline" ("Unsupported raw inline: " <> formatName fmt)]

  _ -> []

modalFlattening :: Text -> [Inline] -> [SCDocWarning]
modalFlattening tag xs
  | any hasNestedMarkup xs =
      [warn "modal-flattening"
        (tag <> ":: can't contain nested tags. Inner markup flattened to plain text.")]
  | otherwise = []

hasNestedMarkup :: Inline -> Bool
hasNestedMarkup = \case
  Str _     -> False
  Space     -> False
  SoftBreak -> False
  LineBreak -> False
  Quoted _ xs -> any hasNestedMarkup xs
  Span (_, [], []) xs -> any hasNestedMarkup xs
  _         -> True

-- | Only flag classes on headers (typos there cause silent failures)
unknownHeaderClass :: [Text] -> [SCDocWarning]
unknownHeaderClass classes =
  [ warn "header-class" ("Unknown header class ignored: " <> cls)
  | cls <- classes
  , not (hasClass cls recognized)
  ]
  where
    recognized =
      [ "method", "argument", "returns", "discussion", "private"
      , "copymethod", "classtree", "anchor", "no-anchor", "keyword"
      -- also accept pandoc-generated heading classes that ain't errors
      , "unnumbered", "unlisted"
      ]

-- Check 3: method structure.

data Scope = Outside | InMethod deriving Eq

checkMethodStructure :: [Block] -> [SCDocWarning]
checkMethodStructure = snd . foldl step (Outside, [])
  where
    step (scope, ws) b = case b of
      Header _ (_, classes, _) _
        | hasClass "method"     classes -> (InMethod, ws)
        | hasClass "argument"   classes -> (scope, ws <> unless InMethod scope "argument::")
        | hasClass "returns"    classes -> (scope, ws <> unless InMethod scope "returns::")
        | hasClass "discussion" classes -> (scope, ws <> unless InMethod scope "discussion::")
      Header 1 _ _ -> (Outside, ws)   -- top-level heading resets scope
      Header 2 _ _ -> (Outside, ws)
      _            -> (scope, ws)

    unless expected actual tag
      | expected == actual = []
      | otherwise =
          [warn "method-structure"
            (tag <> " appears outside any method:: block.")]

-- lightweight rendering to detect literal '||' inside a table cell, ayy
-- We don't import the real renderer to avoid cycle, ayy
renderRough :: [Block] -> Text
renderRough = T.concat . map rough
  where
    rough = \case
      Plain xs       -> roughInlines xs
      Para xs        -> roughInlines xs
      LineBlock ls   -> T.intercalate " " (map roughInlines ls)
      CodeBlock _ s  -> s
      RawBlock _ s   -> s
      BlockQuote bs  -> renderRough bs
      BulletList xs  -> T.concat (map renderRough xs)
      OrderedList _ xs -> T.concat (map renderRough xs)
      _              -> ""

    roughInlines = T.concat . map roughInline

    roughInline = \case
      Str s      -> s
      Space      -> " "
      SoftBreak  -> " "
      LineBreak  -> " "
      Code _ s   -> s
      Math _ s   -> s
      RawInline _ s -> s
      Emph xs    -> roughInlines xs
      Strong xs  -> roughInlines xs
      _          -> ""

formatName :: Format -> Text
formatName (Format f) = f
