{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pandoc writer for SuperCollider SCDoc help files.
module Text.Pandoc.Writers.SCDoc
  ( writeSCDoc
  , writeSCDocPure
  , inferMethodContext
  , hasClass
  , isRawSCDocFormat
  , isExternalUrl
  , linkHasAnchor
  , escapeInline
  , escapeBlockBody
  , oneLine
  , normalize
  , normalizeLinkTarget
  , extractDocInfoMeta
  , extractLeadingOrgMeta
  ) where

import           Data.List                  (nub, partition)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import qualified Text.DocLayout             as DL
import           Text.Pandoc.Class          (PandocMonad)
import           Text.Pandoc.Definition
import           Text.Pandoc.Options        (WriterOptions (..))
import           Text.Pandoc.SCDoc.Link     (isExternalUrl, linkHasAnchor,
                                             normalizeImageTarget,
                                             normalizeLinkTarget,
                                             shouldOmitLinkLabel)
import           Text.Pandoc.Shared         (stringify)
import           Text.Pandoc.Templates      (renderTemplate)
import           Text.Pandoc.Walk           (query)
import           Text.Pandoc.Writers.Shared (defField, metaToContext)


-- | Pandoc writer entry point for SCDoc.
writeSCDoc :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeSCDoc opts pd = do
  let (meta', blocks') = preprocessPandoc pd
  ctx <- metaToContext opts
           (pure . DL.literal . renderBlocks)
           (pure . DL.literal . renderInlines)
           meta'
  let body    = renderCore meta' blocks'
      context = defField "body" (DL.literal body)
              $ writerVariables opts <> ctx
  pure $ case writerTemplate opts of
    Nothing  -> body
    Just tpl -> DL.render Nothing (renderTemplate tpl context)


-- | Pure SCDoc renderer without templates or writer options — no monad,
-- no config.
writeSCDocPure :: Pandoc -> Text
writeSCDocPure = render

render :: Pandoc -> Text
render x =
  let (meta', blocks') = preprocessPandoc x
  in  renderCore meta' blocks'


preprocessPandoc :: Pandoc -> (Meta, [Block])
preprocessPandoc (Pandoc meta blocks) =
  let (meta1, blocks1) = extractDocInfoMeta meta blocks
  in  extractLeadingOrgMeta meta1 blocks1

renderCore :: Meta -> [Block] -> Text
renderCore meta blocks =
  T.intercalate "\n" (scHeader meta)
  <> "\n\n"
  <> renderBlocks renderableBlocks
  <> "\n"
  where
    warningDirectiveBlocks = rewriteWarningHeadingDirectives blocks
    methodContextBlocks    = inferMethodContext warningDirectiveBlocks
    localAnchorTargets     = collectLocalAnchorTargets methodContextBlocks
    autoAnchoredBlocks     =
      insertAutoAnchors localAnchorTargets methodContextBlocks
    renderableBlocks       = insertKeywordBlocks meta autoAnchoredBlocks

-- | Turn a plain heading named @warning@ into a one-block
-- @warning::@ directive. Explicit SCDoc structural headings are left
-- alone, so @method:: warning@ remains a method.
rewriteWarningHeadingDirectives :: [Block] -> [Block]
rewriteWarningHeadingDirectives = rewrite
  where
    rewrite [] = []
    rewrite (Header _ (ident, classes, kvs) xs : rest)
      | isWarningHeading xs
      , not (isStructuralHeadingClass classes) =
          let (warningBody, remainingBlocks) = splitWarningBodyBlock rest
              warningClasses                 = if hasClass "warning" classes
                                               then classes
                                               else classes ++ ["warning"]
          in  Div (ident, warningClasses, kvs) (rewrite warningBody)
              : rewrite remainingBlocks
    rewrite (b : rest) = b : rewrite rest

    isWarningHeading xs = normalizeKeyword (plainText xs) == "warning"

    splitWarningBodyBlock []                    = ([], [])
    splitWarningBodyBlock bs@(Header _ _ _ : _) = ([], bs)
    splitWarningBodyBlock (b : bs)              = ([b], bs)


-- | True when a heading already carries SCDoc meaning or anchor control.
hasExplicitSCDocClass :: [Text] -> Bool
hasExplicitSCDocClass cls =
  isStructuralHeadingClass cls
  || hasClass "no-anchor" cls

-- | Classes that render as explicit SCDoc declarations. Unlike
-- 'hasExplicitSCDocClass', this excludes @no-anchor@ so
-- @## warning {.no-anchor}@ still becomes @warning::@.
isStructuralHeadingClass :: [Text] -> Bool
isStructuralHeadingClass cls =
  any (`hasClass` cls)
    ["method", "argument", "returns", "discussion", "private",
     "copymethod", "classtree", "anchor",
     "section", "subsection", "subsubsection"]

-- | State for method and argument inference.
data MethodCtx
  = OutsideSection
  | InsideSection
  | InsideMethod
  deriving Eq

-- | Walk the AST to tag headings with method/argument context before rendering.
inferMethodContext :: [Block] -> [Block]
inferMethodContext = go OutsideSection
  where
    go _ [] = []
    go _ (Header 1 attr xs : rest)
      | isMethodSection xs = Header 1 attr xs : go InsideSection rest
      | otherwise          = Header 1 attr xs : go OutsideSection rest
    go ctx (Header 2 attr@(i, cls, kvs) xs : rest)
      | hasClass "method" cls
      = Header 2 attr xs : go InsideMethod rest
      | ctx /= OutsideSection, not (hasExplicitSCDocClass cls), not (isKeyword xs)
      = Header 2 (i, cls ++ ["method"], kvs) xs : go InsideMethod rest
      | otherwise
      = Header 2 attr xs : go (resetMethod ctx) rest
    go ctx (Header 3 attr@(i, cls, kvs) xs : rest)
      | ctx == InsideMethod, not (hasExplicitSCDocClass cls), not (isKeyword xs)
      = Header 3 (i, cls ++ ["argument"], kvs) xs : go ctx rest
      | otherwise
      = Header 3 attr xs : go ctx rest
    go ctx (b : rest) = b : go ctx rest

    resetMethod OutsideSection = OutsideSection
    resetMethod _              = InsideSection

    isMethodSection xs =
      normalizeKeyword (plainText xs) `elem` ["classmethods", "instancemethods"]

    isKeyword xs = normalize (plainText xs) `elem` ["discussion", "returns"]

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
metaValueText (MetaInlines xs) = plainText xs
metaValueText (MetaBlocks _)   = ""
metaValueText (MetaList xs)    =
  T.intercalate ", " . filter (not . T.null) $ fmap (T.strip . metaValueText) xs
metaValueText (MetaBool _)     = ""
metaValueText (MetaMap _)      = ""

scdocMetaFields :: [Text]
scdocMetaFields =
  ["title", "summary", "categories", "related", "redirect", "keyword", "keywords"]

buildMetaMap :: [(Text, Text)] -> Map.Map Text MetaValue
buildMetaMap pairs =
  Map.fromListWith combineMeta
    [ (k, toDocInfoValue k v)
    | (k, v) <- pairs
    , k `elem` scdocMetaFields
    , not (T.null v)
    ]


-- | Lift a leading RST docinfo field list into 'Meta'.
extractDocInfoMeta :: Meta -> [Block] -> (Meta, [Block])
extractDocInfoMeta meta (DefinitionList defs : rest)
  | not (null recognized) =
      let pairs    = [ (normalize (plainText keyInlines), T.strip (docInfoBlocksText defBlocks))
                     | (keyInlines, defBlocks) <- recognized
                     ]
          merged   = Meta (Map.union (unMeta meta) (buildMetaMap pairs))
          bodyRest = case unknown of
                       []    -> rest
                       (_:_) -> DefinitionList unknown : rest
      in  (merged, bodyRest)
  where
    (recognized, unknown) =
      partition ((`elem` scdocMetaFields) . normalize . plainText . fst) defs
extractDocInfoMeta meta blocks = (meta, blocks)

-- | Lift leading org-mode @#+KEY:@ lines into 'Meta'.
extractLeadingOrgMeta :: Meta -> [Block] -> (Meta, [Block])
extractLeadingOrgMeta meta blocks =
  case span isRawOrgLine blocks of
    ([], _)          -> (meta, blocks)
    (rawLines, rest) ->
      let pairs  = mapMaybe parseOrgLine rawLines
          merged = Meta (Map.union (unMeta meta) (buildMetaMap pairs))
      in  (merged, rest)
  where
    isRawOrgLine (RawBlock (Format "org") t) = "#+" `T.isPrefixOf` T.stripStart t
    isRawOrgLine _                           = False

    parseOrgLine (RawBlock (Format "org") t) = do
      body <- T.stripPrefix "#+" (T.stripStart t)
      let (k, v) = T.breakOn ":" body
      if not (T.null k) && not (T.null (T.strip v))
        then Just (normalize k, T.strip (T.drop 1 v))
        else Nothing
    parseOrgLine _ = Nothing

toDocInfoValue :: Text -> Text -> MetaValue
toDocInfoValue key val
  | key `elem` ["keyword", "keywords"] = MetaList (map MetaString (splitCsv val))
  | otherwise                          = MetaString val

combineMeta :: MetaValue -> MetaValue -> MetaValue
combineMeta (MetaList xs) (MetaList ys) = MetaList (ys <> xs)
combineMeta new _old                    = new

docInfoBlocksText :: [[Block]] -> Text
docInfoBlocksText =
  oneLine . T.intercalate " " . fmap docInfoDefText

docInfoDefText :: [Block] -> Text
docInfoDefText =
  oneLine . T.intercalate " " . fmap blockText
  where
    blockText (Plain xs)     = plainText xs
    blockText (Para xs)      = plainText xs
    blockText (LineBlock ls) = oneLine (T.intercalate " " (fmap plainText ls))
    blockText _              = ""

-- | Insert one @keyword::@ raw block per metadata keyword.
insertKeywordBlocks :: Meta -> [Block] -> [Block]
insertKeywordBlocks meta blocks =
  case kws of
    [] -> blocks
    _  -> insertAfterDescription
            [ RawBlock (Format "schelp") ("keyword:: " <> kw) | kw <- kws ]
            blocks
  where
    kws = nub
        . filter (not . T.null)
        . fmap T.strip
        . concat
        $ [ valueToList v
          | key <- ["keyword", "keywords"]
          , Just v <- [lookupMeta key meta]
          ]

    valueToList (MetaList xs) =
      filter (not . T.null) (map (T.strip . metaValueText) xs)
    valueToList v = splitCsv (metaValueText v)

    insertAfterDescription injected bs =
      case break isDescription bs of
        (pre, d:rest) -> pre <> (d : injected) <> rest
        (pre, [])     -> pre <> injected

    isDescription (Header 1 _ xs) =
      normalize (plainText xs) == "description"
    isDescription _ = False

-- | Collect anchor names from local @#anchor@ links in the document.
collectLocalAnchorTargets :: [Block] -> [Text]
collectLocalAnchorTargets blocks = Set.toList (query extractAnchor blocks)
  where
    extractAnchor :: Inline -> Set.Set Text
    extractAnchor (Link _ _ (url, _)) =
      let t = T.strip url
      in  if "#" `T.isPrefixOf` t && T.length t > 1
          then Set.singleton (T.drop 1 t)
          else Set.empty
    extractAnchor _ = Set.empty


-- | Insert an @anchor::name::@ tag after each heading referenced by a local
-- link. Headings get tagged only when something points to them — no orphan
-- anchors.
insertAutoAnchors :: [Text] -> [Block] -> [Block]
insertAutoAnchors targets =
  concatMap insertOne
  where
    targetSet = Set.fromList (fmap T.strip targets)

    insertOne h@(Header _ (ident, classes, _) _)
      | not (T.null ident')
      , Set.member ident' targetSet
      , not (hasClass "no-anchor" classes)
      = [h, RawBlock (Format "schelp") ("anchor::" <> ident' <> "::")]
      where ident' = T.strip ident
    insertOne b = [b]

-- Blocks

renderBlocks :: [Block] -> Text
renderBlocks =
  T.intercalate "\n\n"
  . filter (not . T.null . T.strip)
  . fmap renderBlock

renderBlock :: Block -> Text
renderBlock = \case
  Plain xs        -> renderInlines xs
  -- Markdown display math arrives as a paragraph containing one
  -- DisplayMath inline; render that as a block-level @math::@ tag.
  Para [Math DisplayMath s] -> blockTag "math" s
  Para xs         -> renderInlines xs
  LineBlock ls    -> T.intercalate "\n" (fmap renderInlines ls)

  RawBlock fmt s
    -- Raw SCDoc blocks own their body; trim the trailing separator.
    | isRawSCDocFormat fmt -> T.stripEnd s
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
    "definitionlist::\n" <> T.concat (fmap defRow defs) <> "::"

  Header level attr@(_, classes, _) xs
    | hasClass "method"     classes -> renderMethod attr xs
    | hasClass "argument"   classes -> "argument:: " <> attrOrHeading ["name"] attr xs
    | hasClass "returns"    classes -> renderReturns attr
    | hasClass "discussion" classes -> "discussion::"
    | hasClass "private"    classes ->
        "private:: " <> normalizePrivateNames (attrOrHeading ["name"] attr xs)
    | hasClass "copymethod" classes -> renderCopyMethod attr xs
    | hasClass "classtree"  classes -> "classtree:: " <> attrOrHeading ["name"] attr xs
    | hasClass "anchor"     classes -> modalTag "anchor" (attrOrHeading ["name"] attr xs)
    | normalize (plainText xs) == "discussion" -> "discussion::"
    | normalize (plainText xs) == "returns"    -> renderReturns attr
    | otherwise                                -> renderStructuralHeader level attr xs

  HorizontalRule -> ""

  Table _ _ _ (TableHead _ hr) bodies (TableFoot _ fr) ->
    "table::\n" <> T.concat (map renderRow (hr <> concatMap bodyRows bodies <> fr)) <> "::"
    where
      bodyRows (TableBody _ _ h r) = h <> r

      -- Use compact rows only for single-line cells. Any block body or
      -- newline-producing inline needs the multiline cell layout.
      renderRow (Row _ cells)
        | not (all (isSimpleCellBlocks . cellBlocks) cells) =
            "##\n" <> T.intercalate "\n||\n" (fmap renderCellBlock cells) <> "\n"
        | otherwise =
            "## " <> T.intercalate " || " (fmap renderCellInline cells) <> "\n"

      cellBlocks (Cell _ _ _ _ bs) = bs

      renderCellInline (Cell _ _ _ _ bs) = oneLine (renderBlocks bs)

      -- Multiline cell layout preserves block and line breaks.
      renderCellBlock (Cell _ _ _ _ bs) = case bs of
        [] -> ""
        _  -> T.stripEnd (renderBlocks bs)

      -- Compact cells must render as one line; otherwise @||@ row
      -- parsing becomes ambiguous or structure is collapsed.
      isSimpleCellBlocks []         = True
      isSimpleCellBlocks [Para xs]  = not (rendersWithNewline xs)
      isSimpleCellBlocks [Plain xs] = not (rendersWithNewline xs)
      isSimpleCellBlocks _          = False

      rendersWithNewline :: [Inline] -> Bool
      rendersWithNewline xs = T.any (== '\n') (renderInlines xs)

  Figure _ (Caption _ cap) bs ->
    case bs of
      [Plain [Image imgAttr _ target]] -> renderImage imgAttr capInlines target
      [Para  [Image imgAttr _ target]] -> renderImage imgAttr capInlines target
      _                                ->
        renderBlocks bs
        <> if null cap then "" else "\n\n" <> renderBlocks cap
    where
      capInlines = concatMap toInlines cap
      toInlines (Para  xs) = xs
      toInlines (Plain xs) = xs
      toInlines _          = []

  Div (_, classes, _) bs
    | hasClass "note"    classes -> rangeTag "note"    (renderBlocks (stripAdmonitionTitle bs))
    | hasClass "warning" classes -> rangeTag "warning" (renderBlocks (stripAdmonitionTitle bs))
    | hasClass "tree"    classes -> renderTreeDiv bs
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

renderCopyMethod :: Attr -> [Inline] -> Text
renderCopyMethod attr xs =
  case (attrText ["class", "source"] attr, attrText ["method"] attr) of
    (Just cls, Just meth) -> copy cls meth
    _                     ->
      case parseCopyMethodLabel (plainText xs) of
        Just (cls, meth) -> copy cls meth
        -- If the label is not splittable, keep the literal
        -- @copymethod::@ line instead of inventing a @method::@.
        Nothing          -> "copymethod:: " <> oneLine (renderInlines xs)
  where
    copy cls meth = "copymethod:: " <> T.strip cls <> " " <> T.strip meth

-- | Parse @Class.method@ or @Class method...@ labels.
parseCopyMethodLabel :: Text -> Maybe (Text, Text)
parseCopyMethodLabel label =
  case T.words (T.strip label) of
    [single]    -> parseDotted single
    (cls : ms@(_:_))
      | "." `T.isInfixOf` cls -> Nothing
      | otherwise             -> Just (cls, T.unwords ms)
    _           -> Nothing
  where
    parseDotted t =
      case T.breakOnEnd "." (T.strip t) of
        (clsDot, meth)
          | not (T.null clsDot), not (T.null meth) ->
              Just (T.dropEnd 1 clsDot, meth)
        _ -> Nothing

renderStructuralHeader :: Int -> Attr -> [Inline] -> Text
renderStructuralHeader level attr@(_, _, kvs) xs =
  let title  = attrOrHeading ["title"] attr xs
      header = case level of
        1 -> case normalizeKeyword title of
               "description"     -> "description::"
               "classmethods"    -> "classmethods::"
               "instancemethods" -> "instancemethods::"
               "examples"        -> "examples::"
               _                 -> "section:: " <> title
        2 -> "subsection:: "    <> title
        _ -> "subsubsection:: " <> title
  in case lookupNonEmpty "anchor" kvs of
       Just a  -> header <> "\n\n" <> modalTag "anchor" a
       Nothing -> header

-- Inlines

renderInlines :: [Inline] -> Text
renderInlines = T.concat . fmap renderInline

renderInline :: Inline -> Text
renderInline = \case
  -- Plain text can contain literal SCDoc delimiters.
  Str s            -> escapeInline s
  Space            -> " "
  SoftBreak        -> " "
  LineBreak        -> "\n"

  Emph xs          -> modalTag "emphasis" (plainText xs)
  Strong xs        -> modalTag "strong"   (plainText xs)
  Strikeout xs     -> modalTag "soft"     (plainText xs)

  -- No SCDoc equivalent, just pass through.
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
  Image attr alt target -> renderImage attr alt target
  Note bs             -> "footnote::\n" <> renderBlocks bs <> "\n::"

  Span (_, classes, _) xs
    | hasClass "soft"     classes -> modalTag "soft"     (plainText xs)
    | hasClass "teletype" classes -> modalTag "teletype" (plainText xs)
    | otherwise                   -> renderInlines xs

renderLink :: [Inline] -> (Text, Text) -> Text
renderLink label (url, _) =
  modalTag "link" body
  where
    target = normalizeLinkTarget (T.strip url)
    lab    = T.strip (plainText label)

    -- For anchored links, labels containing @#@ need the unambiguous
    -- @##@ separator.
    body
      | shouldOmitLinkLabel target lab            = target
      | T.null target                             = "##" <> lab
      | isExternalUrl target                      = target <> "##" <> lab
      | linkHasAnchor target
      , not ("#" `T.isInfixOf` lab)               = target <> "#"  <> lab
      | otherwise                                 = target <> "##" <> lab

renderImage :: Attr -> [Inline] -> (Text, Text) -> Text
renderImage (_, _, kvs) alt (url, _) =
  modalTag "image" body
  where
    target  = normalizeImageTarget (T.strip url)
    caption = T.strip (plainText alt)
    mlink   = lookupNonEmpty "link" kvs
    body = case mlink of
      Nothing  | T.null caption -> target
               | otherwise      -> target <> "#" <> caption
      Just lnk | T.null caption -> target <> "##" <> lnk
               | otherwise      -> target <> "#" <> caption <> "#" <> lnk

renderTreeDiv :: [Block] -> Text
renderTreeDiv bs =
  case concatMap extractItems bs of
    []    -> renderBlocks bs
    items -> listTag "tree" items
  where
    extractItems (BulletList    xs) = xs
    extractItems (OrderedList _ xs) = xs
    extractItems _                  = []

-- Tags

modalTag :: Text -> Text -> Text
modalTag tag s =
  tag <> "::" <> openSep <> escaped <> closeSep <> "::"
  where
    escaped = escapeInline s
    -- Keep body text from merging with opener/closer @::@. The reader
    -- strips these separator spaces on re-parse.
    openSep
      | not (T.null escaped), T.head escaped == ':'  = " "
      | otherwise                                    = ""
    closeSep
      | "\\" `T.isSuffixOf` escaped                  = " "
      | ":"  `T.isSuffixOf` escaped                  = " "
      | otherwise                                    = ""

blockTag :: Text -> Text -> Text
blockTag tag s = tag <> "::\n" <> escapeBlockBody (T.stripEnd s) <> "\n::"

rangeTag :: Text -> Text -> Text
rangeTag tag s = tag <> "::\n" <> s <> "\n::"

listTag :: Text -> [[Block]] -> Text
listTag tag items =
  tag <> "::\n" <> T.concat (fmap renderListItem items) <> "::"

-- | Render one list item as a single @##@ entry with a full block body.
-- Empty rendered items are omitted.
renderListItem :: [Block] -> Text
renderListItem item =
  case body of
    "" -> ""
    _  -> "## " <> body <> "\n"
  where
    body = T.intercalate "\n\n"
         . filter (not . T.null)
         $ fmap (T.strip . renderBlock) item

stripAdmonitionTitle :: [Block] -> [Block]
stripAdmonitionTitle (Div (_, cls, _) _ : rest) | hasClass "title" cls = rest
stripAdmonitionTitle bs = bs

-- | Emit one @defterms BARS optbody@ row. Grouped reader terms share
-- one body; plain terms with multiple definitions become multiple rows.
defRow :: ([Inline], [[Block]]) -> Text
defRow (term, defs) = case extractDeftermGroup term of
  Just terms | [body] <- defs ->
    headers terms <> "|| " <> renderBlocks body <> "\n"
  Just terms | null defs      ->
    headers terms <> "||\n"
  _                           ->
    if null defs
      then "## " <> termText <> "\n||\n"
      else T.concat
             [ "## " <> termText <> "\n|| " <> renderBlocks d <> "\n"
             | d <- defs
             ]
  where
    termText  = oneLine (renderInlines term)
    headers ts = T.concat [ "## " <> oneLine (renderInlines t) <> "\n"
                          | t <- ts ]

-- | Recognize the reader's multi-term shared-body marker.
extractDeftermGroup :: [Inline] -> Maybe [[Inline]]
extractDeftermGroup = go []
  where
    go acc [Span (_, cls, _) ts] | hasClass "scdoc-defterm" cls =
      let result = reverse (ts : acc)
      in if length result >= 2 then Just result else Nothing
    go acc (Span (_, cls, _) ts : LineBreak : rest)
      | hasClass "scdoc-defterm" cls = go (ts : acc) rest
    go _ _ = Nothing

attrText :: [Text] -> Attr -> Maybe Text
attrText keys (_, _, kvs) =
  listToMaybe (mapMaybe (`lookupNonEmpty` kvs) keys)

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
      let r = plainText ys in if T.null r then i else r

-- | Normalize comma-separated @method::@ names, including setter @_@ stripping.
normalizeMethodNames :: Text -> Text
normalizeMethodNames = normalizeNames True

-- | Normalize comma-separated @private::@ names without setter @_@ stripping.
normalizePrivateNames :: Text -> Text
normalizePrivateNames = normalizeNames False

normalizeNames :: Bool -> Text -> Text
normalizeNames dropUnderscore =
  T.intercalate ", "
  . fmap (if dropUnderscore then dropSetterUnderscore else id)
  . splitCsv
  where
    dropSetterUnderscore name
      | T.length name > 1, "_" `T.isSuffixOf` name = T.dropEnd 1 name
      | otherwise                                  = name

-- Utils

-- | Case-insensitive class test.
hasClass :: Text -> [Text] -> Bool
hasClass c = elem (T.toLower c) . fmap T.toLower

-- | Test whether a raw Pandoc format should pass through as SCDoc.
isRawSCDocFormat :: Format -> Bool
isRawSCDocFormat (Format f) = T.toLower f `elem` ["schelp", "scdoc"]

-- | Collapse whitespace to a single line.
oneLine :: Text -> Text
oneLine = T.unwords . T.words

-- | Split @a, b, c@ into trimmed, non-empty fields.
splitCsv :: Text -> [Text]
splitCsv = filter (not . T.null) . fmap T.strip . T.splitOn ","

plainText :: [Inline] -> Text
plainText = oneLine . stringify

-- | Lowercase text after applying 'oneLine'.
normalize :: Text -> Text
normalize = T.toLower . oneLine

-- | Like 'normalize' but also strips spaces, so "Class Methods" and
-- "classmethods" compare equal.
normalizeKeyword :: Text -> Text
normalizeKeyword = T.filter (/= ' ') . normalize

-- | Escape inline modal bodies for the reader's verbatim unescapes.
-- Backslash-prefixed escapes must be doubled before @::@ is escaped.
escapeInline :: Text -> Text
escapeInline =
    T.replace "::"   "\\::"
  . T.replace "\\##" "\\\\##"
  . T.replace "\\||" "\\\\||"

-- | Escape block modal bodies. Lines that are exactly @::@ must be
-- escaped because they close the block.
--
-- Limitation: SCDoc cannot preserve block-body lines whose first
-- non-whitespace text is @\\::@; the lexer consumes that as an
-- escaped close.
escapeBlockBody :: Text -> Text
escapeBlockBody =
    T.intercalate "\n"
  . fmap escapeLine
  . T.splitOn "\n"
  . T.replace "\\##" "\\\\##"
  . T.replace "\\||" "\\\\||"
  where
    escapeLine line | T.strip line == "::" = "\\::"
                    | otherwise            = line
