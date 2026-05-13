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
  <> renderBlocks blocksWithMetadata
  <> "\n"
  where
    inferred           = inferMethodContext blocks
    referenced         = referencedLocalAnchors inferred
    blocksWithAnchors  = insertReferencedHeadingAnchors referenced inferred
    blocksWithMetadata = insertKeywordBlocks meta blocksWithAnchors

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

    hasExplicitSCDocClass cls =
      any (`hasClass` cls)
        ["method", "argument", "returns", "discussion", "private",
         "copymethod", "classtree", "anchor", "no-anchor"]

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
  | key `elem` ["keyword", "keywords"] =
      MetaList
        . fmap (MetaString . T.strip)
        . filter (not . T.null . T.strip)
        $ T.splitOn "," val
  | otherwise = MetaString val

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

-- | Insert @keyword::@ blocks from keyword metadata.
insertKeywordBlocks :: Meta -> [Block] -> [Block]
insertKeywordBlocks meta blocks =
  case kws of
    [] -> blocks
    _  -> insertAfterDescription
            [RawBlock (Format "schelp")
              (T.intercalate "\n" (fmap ("keyword:: " <>) kws))]
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
    valueToList v =
      filter (not . T.null . T.strip) . fmap T.strip . T.splitOn "," $ metaValueText v

    insertAfterDescription injected bs =
      case break isDescription bs of
        (pre, d:rest) -> pre <> (d : injected) <> rest
        (pre, [])     -> pre <> injected

    isDescription (Header 1 _ xs) =
      normalize (plainText xs) == "description"
    isDescription _ = False

-- | Collect anchor names from local @#anchor@ links in the document.
referencedLocalAnchors :: [Block] -> [Text]
referencedLocalAnchors blocks = Set.toList (query extractAnchor blocks)
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
insertReferencedHeadingAnchors :: [Text] -> [Block] -> [Block]
insertReferencedHeadingAnchors targets =
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
  Para xs         -> renderInlines xs
  LineBlock ls    -> T.intercalate "\n" (fmap renderInlines ls)

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
    "definitionlist::\n" <> T.concat (fmap defRow defs) <> "::"

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
    | normalize (plainText xs) == "discussion" -> "discussion::"
    | normalize (plainText xs) == "returns"    -> renderReturns attr
    | otherwise                                -> renderStructuralHeader level attr xs

  HorizontalRule -> ""

  Table _ _ _ (TableHead _ hr) bodies (TableFoot _ fr) ->
    "table::\n" <> T.concat (map renderRow (hr <> concatMap bodyRows bodies <> fr)) <> "::"
    where
      bodyRows (TableBody _ _ h r) = h <> r
      renderRow (Row _ cells) =
        "## " <> T.intercalate " || " (fmap renderCell cells) <> "\n"
      renderCell (Cell _ _ _ _ bs) = oneLine (renderBlocks bs)

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
        Nothing          -> "method:: " <> normalizeMethodNames (plainText xs)
  where
    copy cls meth = "copymethod:: " <> T.strip cls <> " " <> T.strip meth

parseCopyMethodLabel :: Text -> Maybe (Text, Text)
parseCopyMethodLabel label =
  case T.words (T.strip label) of
    [single]   -> parseDotted single
    [cls, meth]
      | "." `T.isInfixOf` cls -> Nothing
      | otherwise             -> Just (cls, meth)
    _          -> Nothing
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
  Str s            -> s
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

    body
      | shouldOmitLinkLabel target lab = target
      | T.null target                  = "##" <> lab
      | isExternalUrl target           = target <> "##" <> lab
      | linkHasAnchor target           = target <> "#"  <> lab
      | otherwise                      = target <> "##" <> lab

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
modalTag tag s = tag <> "::" <> escapeInline s <> "::"

blockTag :: Text -> Text -> Text
blockTag tag s = tag <> "::\n" <> escapeBlockBody (T.stripEnd s) <> "\n::"

rangeTag :: Text -> Text -> Text
rangeTag tag s = tag <> "::\n" <> s <> "\n::"

listTag :: Text -> [[Block]] -> Text
listTag tag items =
  tag <> "::\n"
  <> T.concat [ "## " <> t <> "\n" | item <- items, t <- flattenItem item ]
  <> "::"

-- | Collect leaf text lines from a list item, flattening nested lists.
-- SCDoc has no nested-list syntax — so we flatten.
flattenItem :: [Block] -> [Text]
flattenItem = concatMap go
  where
    go (BulletList    nested) = concatMap flattenItem nested
    go (OrderedList _ nested) = concatMap flattenItem nested
    go b =
      let t = T.strip (renderBlock b)
      in  [t | not (T.null t)]

stripAdmonitionTitle :: [Block] -> [Block]
stripAdmonitionTitle (Div (_, cls, _) _ : rest) | hasClass "title" cls = rest
stripAdmonitionTitle bs = bs

defRow :: ([Inline], [[Block]]) -> Text
defRow (term, []) =
  "## " <> oneLine (renderInlines term) <> "\n||\n"
defRow (term, defs) =
  T.concat
    [ "## " <> termText <> "\n|| " <> oneLine (renderBlocks d) <> "\n"
    | d <- defs
    ]
  where termText = oneLine (renderInlines term)

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

normalizeMethodNames :: Text -> Text
normalizeMethodNames =
  T.intercalate ", "
  . fmap dropSetterUnderscore
  . filter (not . T.null)
  . fmap T.strip
  . T.splitOn ","
  where
    dropSetterUnderscore name
      | T.length name > 1, "_" `T.isSuffixOf` name = T.dropEnd 1 name
      | otherwise                                  = name

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

-- | Normalize an internal SCDoc link target.
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
    (x:_) -> dropAnyPrefix prefixes x
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

plainText :: [Inline] -> Text
plainText = oneLine . stringify

-- | Lowercase text after applying 'oneLine'.
normalize :: Text -> Text
normalize = T.toLower . oneLine

-- | Like 'normalize' but also strips spaces, so "Class Methods" and
-- "classmethods" compare equal.
normalizeKeyword :: Text -> Text
normalizeKeyword = T.filter (/= ' ') . normalize

escapeInline :: Text -> Text
escapeInline = T.replace "::" "\\::"

escapeBlockBody :: Text -> Text
escapeBlockBody =
  T.intercalate "\n" . fmap escape . T.splitOn "\n"
  where
    escape line | T.strip line == "::" = "\\::"
                | otherwise            = line
