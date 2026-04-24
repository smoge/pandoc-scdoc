{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Writers.SCDoc.Render
  ( render
  , hasClass
  , isRawSCDocFormat
  , isExternalUrl
  , linkHasAnchor
  ) where

import           Data.List                       (nub)
import           Data.Maybe                      (fromMaybe, mapMaybe,
                                                  maybeToList)
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import           Text.Pandoc.Definition

import           Text.Pandoc.Writers.SCDoc.Plain (plainTextInlines)


render :: Pandoc -> Text
render (Pandoc meta blocks) =
  T.intercalate "\n" (scHeader meta)
  <> "\n\n"
  <> renderBlocks blocksWithMetadata
  <> "\n"
  where
    referenced        = referencedLocalAnchors blocks
    blocksWithAnchors = insertReferencedHeadingAnchors referenced blocks
    blocksWithMetadata = insertKeywordBlocks meta blocksWithAnchors



scHeader :: Meta -> [Text]
scHeader meta =
  mapMaybe field ["title", "summary", "categories", "related", "redirect"]
  where
    field k = do
      v <- T.strip <$> metaToLine meta k
      if T.null v
        then Nothing
        else pure (k <> ":: " <> v)

metaToLine :: Meta -> Text -> Maybe Text
metaToLine meta key =
  oneLine . metaValueText <$> lookupMeta key meta

metaValueText :: MetaValue -> Text
metaValueText (MetaString s)   = s
metaValueText (MetaInlines xs) = plainTextInlines xs
metaValueText (MetaBlocks _)   = ""
metaValueText (MetaList xs)    =
  T.intercalate ", " . filter (not . T.null) $ map (T.strip . metaValueText) xs
metaValueText (MetaBool b)     = if b then "true" else "false"
metaValueText (MetaMap _)      = ""

-- | keyword:: is a body tag.keyword or keywords is inserted
-- into the body preferably right after description::
insertKeywordBlocks :: Meta -> [Block] -> [Block]
insertKeywordBlocks meta blocks =
  case kws of
    [] -> blocks
    _  -> insertAfterDescription
            [RawBlock (Format "schelp") ("keyword:: " <> T.intercalate ", " kws)]
            blocks
  where
    kws = nub
        . filter (not . T.null)
        . map T.strip
        . concat
        $ [ valueToList v
          | key <- ["keyword", "keywords"]
          , Just v <- [lookupMeta key meta]
          ]

    valueToList (MetaList xs) =
      filter (not . T.null) (map (T.strip . metaValueText) xs)
    valueToList v =
      let x = T.strip (metaValueText v) in [x | not (T.null x)]

    insertAfterDescription injected bs =
      case break isDescription bs of
        (pre, d:rest) -> pre <> (d : injected) <> rest
        (pre, [])     -> injected <> pre

    isDescription (Header 1 _ xs) =
      normalize (plainTextInlines xs) == "description"
    isDescription _ = False

-- NOTE: pandoc auto-generates heading identifiers (sometimes too much for us)
-- Emitting every one as `anchor::` produces to much noise in the output.
-- On the other hand, emitting none breaks same-document links.
-- Compromise: emit `anchor::` only for heading IDs that are referenced
-- by a local link somewhere else Needs testing

referencedLocalAnchors :: [Block] -> [Text]
referencedLocalAnchors =
  nub . concatMap blockTargets
  where
    blockTargets = \case
      Plain xs            -> inlineTargets xs
      Para xs             -> inlineTargets xs
      LineBlock ls        -> concatMap inlineTargets ls
      BlockQuote bs       -> concatMap blockTargets bs
      BulletList items    -> concatMap (concatMap blockTargets) items
      OrderedList _ items -> concatMap (concatMap blockTargets) items
      DefinitionList defs -> concatMap defTargets defs
      Header _ _ xs       -> inlineTargets xs
      Table _ _ _ h bs f  -> tableTargets h bs f
      Figure _ (Caption _ cap) bs -> concatMap blockTargets bs
                                  <> concatMap blockTargets cap
      Div _ bs            -> concatMap blockTargets bs
      _                   -> []

    defTargets (term, defs) =
      inlineTargets term <> concatMap (concatMap blockTargets) defs

    tableTargets (TableHead _ hr) bodies (TableFoot _ fr) =
      concatMap rowTargets (hr <> concatMap bodyRows bodies <> fr)
      where
        bodyRows (TableBody _ _ h r) = h <> r
        rowTargets (Row _ cells) = concatMap cellTargets cells
        cellTargets (Cell _ _ _ _ bs) = concatMap blockTargets bs

    inlineTargets = concatMap inlineTarget

    inlineTarget = \case
      Link _ _ (url, _) -> maybeToList (localAnchor url)
      Emph xs           -> inlineTargets xs
      Strong xs         -> inlineTargets xs
      Underline xs      -> inlineTargets xs
      Strikeout xs      -> inlineTargets xs
      Superscript xs    -> inlineTargets xs
      Subscript xs      -> inlineTargets xs
      SmallCaps xs      -> inlineTargets xs
      Quoted _ xs       -> inlineTargets xs
      Cite _ xs         -> inlineTargets xs
      Image _ alt _     -> inlineTargets alt
      Note bs           -> concatMap blockTargets bs
      Span _ xs         -> inlineTargets xs
      _                 -> []

    localAnchor raw =
      case T.strip raw of
        t | "#" `T.isPrefixOf` t, T.length t > 1 -> Just (T.drop 1 t)
          | otherwise                             -> Nothing

insertReferencedHeadingAnchors :: [Text] -> [Block] -> [Block]
insertReferencedHeadingAnchors targets =
  concatMap insertOne
  where
    targetSet = nub (map T.strip targets)

    insertOne h@(Header _ (ident, classes, _) _)
      | not (T.null ident')
      , ident' `elem` targetSet
      , not (hasClass "no-anchor" classes)
      = [h, RawBlock (Format "schelp") ("anchor::" <> ident' <> "::")]
      where ident' = T.strip ident
    insertOne b = [b]


-- BLOCKS

renderBlocks :: [Block] -> Text
renderBlocks =
  T.intercalate "\n\n"
  . filter (not . T.null . T.strip)
  . map renderBlock

renderBlock :: Block -> Text
renderBlock = \case
  Plain xs        -> renderInlines xs
  Para xs         -> renderInlines xs
  LineBlock ls    -> T.intercalate "\n" (map renderInlines ls)

  RawBlock fmt s
    | isRawSCDocFormat fmt -> s
    | otherwise            -> ""

  CodeBlock (_, classes, _) s
    | hasClass "schelp"   classes -> s
    | hasClass "scdoc"    classes -> s
    | hasClass "teletype" classes -> blockTag "teletype" s
    | hasClass "math"     classes -> blockTag "math"     s
    | otherwise                   -> blockTag "code"     s

  BlockQuote bs       -> rangeTag "note" (renderBlocks bs)
  BulletList items    -> listTag "list"         items
  OrderedList _ items -> listTag "numberedlist" items

  DefinitionList defs ->
    "definitionlist::\n" <> T.concat (map defRow defs) <> "::"

  Header level attr@(_, classes, _) xs
    | hasClass "method"     classes -> renderMethod attr xs
    | hasClass "argument"   classes -> "argument:: " <> attrOrHeading ["name"] attr xs
    | hasClass "returns"    classes -> renderReturns attr
    | hasClass "discussion" classes -> "discussion::"
    | hasClass "private"    classes ->
        "private:: " <> normalizeMethodNames (attrOrHeading ["name"] attr xs)
    | hasClass "copymethod" classes -> renderCopyMethod attr xs
    | hasClass "classtree"  classes -> "classtree:: " <> attrOrHeading ["name"] attr xs
    | hasClass "anchor"     classes -> modalTag "anchor" (attrOrHeading ["name"] attr xs)
    | otherwise                     -> renderStructuralHeader level attr xs

  HorizontalRule -> ""

  Table _ _ _ (TableHead _ hr) bodies (TableFoot _ fr) ->
    "table::\n" <> T.concat (map renderRow (hr <> concatMap bodyRows bodies <> fr)) <> "::"
    where
      bodyRows (TableBody _ _ h r) = h <> r
      renderRow (Row _ cells) =
        "## " <> T.intercalate " || " (map renderCell cells) <> "\n"
      renderCell (Cell _ _ _ _ bs) = oneLine (renderBlocks bs)

  Figure _ (Caption _ cap) bs ->
    renderBlocks bs
    <> if null cap then "" else "\n\n" <> renderBlocks cap

  Div (_, classes, _) bs
    | hasClass "note"    classes -> rangeTag "note"    (renderBlocks bs)
    | hasClass "warning" classes -> rangeTag "warning" (renderBlocks bs)
    | otherwise                  -> renderBlocks bs

renderMethod :: Attr -> [Inline] -> Text
renderMethod attr xs =
  "method:: " <> names <> maybe "" ((" " <>) . T.strip) (attrText ["args"] attr)
  where
    names = normalizeMethodNames (attrOrHeading ["name"] attr xs)

renderReturns :: Attr -> Text
renderReturns attr =
  case attrText ["text", "value"] attr of
    Just t  -> "returns:: " <> T.strip t
    Nothing -> "returns::"

-- requires a class and a method name.
renderCopyMethod :: Attr -> [Inline] -> Text
renderCopyMethod attr _xs =
  case (attrText ["class"] attr, attrText ["method"] attr) of
    (Just cls, Just meth) -> "copymethod:: " <> T.strip cls <> " " <> T.strip meth
    _                     -> ""

renderStructuralHeader :: Int -> Attr -> [Inline] -> Text
renderStructuralHeader level attr@(_, _, kvs) xs =
  let title  = attrOrHeading ["title"] attr xs
      header = case level of
        1 -> case normalize title of
               "description"      -> "description::"
               "class methods"    -> "classmethods::"
               "classmethods"     -> "classmethods::"
               "instance methods" -> "instancemethods::"
               "instancemethods"  -> "instancemethods::"
               "examples"         -> "examples::"
               _                  -> "section:: " <> title
        2 -> "subsection:: "    <> title
        _ -> "subsubsection:: " <> title
  in case lookupNonEmpty "anchor" kvs of
       Just a  -> header <> "\n\n" <> modalTag "anchor" a
       Nothing -> header


-- Inlines

renderInlines :: [Inline] -> Text
renderInlines = T.concat . map renderInline

renderInline :: Inline -> Text
renderInline = \case
  Str s            -> s
  Space            -> " "
  SoftBreak        -> " "
  LineBreak        -> "\n"

  -- Modal tags are flattened to plain text
  Emph xs          -> modalTag "emphasis" (plainTextInlines xs)
  Strong xs        -> modalTag "strong"   (plainTextInlines xs)
  Strikeout xs     -> modalTag "soft"     (plainTextInlines xs)

  -- No SCDoc equivalent; just pass through...
  Underline xs     -> renderInlines xs
  Superscript xs   -> renderInlines xs
  Subscript xs     -> renderInlines xs
  SmallCaps xs     -> renderInlines xs
  Cite _ xs        -> renderInlines xs

  Quoted SingleQuote xs -> "'"  <> renderInlines xs <> "'"
  Quoted DoubleQuote xs -> "\"" <> renderInlines xs <> "\""

  Code _ s         -> modalTag "code" s
  Math InlineMath s  -> modalTag "math" s
  Math DisplayMath s -> "\n" <> blockTag "math" s <> "\n"

  RawInline fmt s
    | isRawSCDocFormat fmt -> s
    | otherwise            -> ""

  Link _ label target -> renderLink label target
  Image _ alt target  -> renderImage alt target
  Note bs             -> "footnote::\n" <> renderBlocks bs <> "\n::"

  Span (_, classes, _) xs
    | hasClass "soft"     classes -> modalTag "soft"     (plainTextInlines xs)
    | hasClass "teletype" classes -> modalTag "teletype" (plainTextInlines xs)
    | otherwise                   -> renderInlines xs

renderLink :: [Inline] -> (Text, Text) -> Text
renderLink label (url, _) =
  modalTag "link" body
  where
    target = normalizeLinkTarget (T.strip url)
    lab    = T.strip (plainTextInlines label)

    body
      | shouldOmitLinkLabel target lab = target
      | T.null target                  = "##" <> lab
      | isExternalUrl target           = target <> "##" <> lab
      -- single '#' is scdoc's anchor separator on targets
      -- `##` is the label separator. When the target already carries
      -- an anchor the label attaches with single '#'
      | linkHasAnchor target           = target <> "#"  <> lab
      | otherwise                      = target <> "##" <> lab

renderImage :: [Inline] -> (Text, Text) -> Text
renderImage alt (url, _) =
  modalTag "image" body
  where
    target  = normalizeImageTarget (T.strip url)
    caption = T.strip (plainTextInlines alt)
    body | T.null caption = target
         | otherwise      = target <> "#" <> caption


-- Tags

modalTag :: Text -> Text -> Text
modalTag tag s = tag <> "::" <> escapeInline s <> "::"

blockTag :: Text -> Text -> Text
blockTag tag s = tag <> "::\n" <> escapeBlockBody s <> "\n::"

rangeTag :: Text -> Text -> Text
rangeTag tag s = tag <> "::\n" <> s <> "\n::"

listTag :: Text -> [[Block]] -> Text
listTag tag items =
  tag <> "::\n"
  <> T.concat [ "## " <> renderListItem item <> "\n" | item <- items ]
  <> "::"

renderListItem :: [Block] -> Text
renderListItem bs
  | all isSimple bs = oneLine rendered
  | otherwise       = T.stripEnd rendered
  where
    rendered = renderBlocks bs
    isSimple (Plain _) = True
    isSimple (Para _)  = True
    isSimple _         = False

defRow :: ([Inline], [[Block]]) -> Text
defRow (term, []) =
  "## " <> oneLine (renderInlines term) <> "\n||\n"
defRow (term, defs) =
  T.concat
    [ "## " <> termText <> "\n|| " <> oneLine (renderBlocks d) <> "\n"
    | d <- defs
    ]
  where termText = oneLine (renderInlines term)

-- helpers

attrText :: [Text] -> Attr -> Maybe Text
attrText keys (_, _, kvs) =
  firstJust [lookup k kvs | k <- keys]
  where
    firstJust []              = Nothing
    firstJust (Nothing  : xs) = firstJust xs
    firstJust (Just x   : xs)
      | T.null (T.strip x)    = firstJust xs
      | otherwise             = Just x

lookupNonEmpty :: Text -> [(Text, Text)] -> Maybe Text
lookupNonEmpty key kvs = do
  v <- lookup key kvs
  let stripped = T.strip v
  if T.null stripped then Nothing else Just stripped

attrOrHeading :: [Text] -> Attr -> [Inline] -> Text
attrOrHeading keys attr@(ident, _, _) xs =
  T.strip (fromMaybe (fallback ident xs) (attrText keys attr))
  where
    fallback i ys =
      let r = plainTextInlines ys in if T.null r then i else r

normalizeMethodNames :: Text -> Text
normalizeMethodNames =
  T.intercalate ", "
  . map dropSetterUnderscore
  . filter (not . T.null)
  . map T.strip
  . T.splitOn ","
  where
    dropSetterUnderscore name
      | T.length name > 1, "_" `T.isSuffixOf` name = T.dropEnd 1 name
      | otherwise                                  = name

-- link normalization

shouldOmitLinkLabel :: Text -> Text -> Bool
shouldOmitLinkLabel target label =
  T.null label
  || label == target
  || (not (linkHasAnchor target) && label == defaultLinkLabel target)

defaultLinkLabel :: Text -> Text
defaultLinkLabel target =
  case filter (not . T.null) (T.splitOn "/" (T.takeWhile (/= '#') target)) of
    [] -> ""
    xs -> last xs

normalizeLinkTarget :: Text -> Text
normalizeLinkTarget target
  | isExternalUrl target = target
  | otherwise            = stripKnownExt path <> frag
  where
    (path0, frag) = T.breakOn "#" target
    path          = dropAnyPrefix ["./HelpSource/", "HelpSource/", "./"] path0

normalizeImageTarget :: Text -> Text
normalizeImageTarget =
  dropAnyPrefix ["./HelpSource/", "HelpSource/", "./"]

dropAnyPrefix :: [Text] -> Text -> Text
dropAnyPrefix prefixes s =
  case [T.drop (T.length p) s | p <- prefixes, p `T.isPrefixOf` s] of
    (x:_) -> x
    []    -> s

stripKnownExt :: Text -> Text
stripKnownExt s = foldr stripOne s [".schelp", ".md", ".html"]
  where
    stripOne ext x
      | ext `T.isSuffixOf` T.toLower x = T.dropEnd (T.length ext) x
      | otherwise                      = x

linkHasAnchor :: Text -> Bool
linkHasAnchor target =
  not (isExternalUrl target)
  && "#" `T.isInfixOf` target
  && not ("##" `T.isInfixOf` target)

isExternalUrl :: Text -> Bool
isExternalUrl x =
  any (`T.isPrefixOf` T.toLower x)
    ["http://", "https://", "ftp://", "mailto:", "file://"]


-- utils

hasClass :: Text -> [Text] -> Bool
hasClass c = elem (T.toLower c) . map T.toLower

isRawSCDocFormat :: Format -> Bool
isRawSCDocFormat (Format f) = T.toLower f `elem` ["schelp", "scdoc"]

oneLine :: Text -> Text
oneLine = T.unwords . T.words

normalize :: Text -> Text
normalize = T.toLower . oneLine

escapeInline :: Text -> Text
escapeInline = T.replace "::" "\\::"

-- In block content, a single `::` on its own line closes
-- the enclosing tag prematurely. Escape that case.
escapeBlockBody :: Text -> Text
escapeBlockBody =
  T.intercalate "\n" . map escape . T.splitOn "\n"
  where
    escape line | T.strip line == "::" = "\\::"
                | otherwise            = line
