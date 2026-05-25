{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Token-based parser for SCDoc.
module Text.Pandoc.Readers.SCDoc.Parser
  ( parseDocument
  , ParseError
  ) where

import           Control.Monad                   (unless, void, when)
import           Data.List                       (nub)
import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import           Text.Parsec

import           Text.Pandoc.Definition
import           Text.Pandoc.Readers.SCDoc.Lexer (Located (..), Tok (..))
import           Text.Pandoc.SCDoc.Link          (normalizeLinkTarget,
                                                  shouldOmitLinkLabel)


type Parser     = Parsec [Located Tok] MethodContext


-- | Section context for method/private/copymethod classes.
data MethodContext
  = NoContext           -- ^ outside any top-level section
  | InDescription
  | InClassMethods
  | InInstanceMethods
  | InExamples
  | InGenericSection    -- ^ @section:: …@ with a non-canonical title
  deriving (Eq, Show)


-- | Parse a token stream into a 'Pandoc' document.
parseDocument :: FilePath -> [Located Tok] -> Either ParseError Pandoc
parseDocument = runParser document NoContext

document :: Parser Pandoc
document = do
  skipBlankLines
  pairs  <- many (try metaField)
  skipBlankLines
  blocks <- bodyBlocks
  eof
  -- keyword:: is metadata whether it appears in the header or body.
  let (bodyKwValues, blocks') = extractKeywordBlocks blocks
      meta                    = mergeBodyKeywords bodyKwValues (buildMeta pairs)
  pure (Pandoc meta blocks')


-- | Split body-level @keyword::@ blocks from regular content.
extractKeywordBlocks :: [Block] -> ([Text], [Block])
extractKeywordBlocks = foldr step ([], [])
  where
    step b (kws, bs) = case keywordValuesIn b of
      Just vs -> (vs ++ kws, bs)
      Nothing -> (kws, b : bs)

    keywordValuesIn (RawBlock (Format "schelp") s)
      | Just rest <- T.stripPrefix "keyword::" (T.stripStart s)
      = Just
      . filter (not . T.null)
      . fmap T.strip
      $ T.splitOn "," rest
    keywordValuesIn _ = Nothing


-- | Merge header and body keywords, preserving order and removing duplicates.
mergeBodyKeywords :: [Text] -> Meta -> Meta
mergeBodyKeywords bodyKws (Meta m) =
  let headerKws = case Map.lookup "keywords" m of
        Just (MetaList xs) -> [s | MetaString s <- xs]
        _                  -> []
      merged    = nub (headerKws ++ bodyKws)
  in  if null merged
        then Meta m
        else Meta (Map.insert "keywords" (MetaList (map MetaString merged)) m)


-- ---------------------------------------------------------------------------
-- Token primitives

-- | Consume one token, keeping source positions for errors.
satisfyTok :: (Tok -> Maybe a) -> Parser a
satisfyTok f = tokenPrim showTok updPos test
  where
    showTok (Located _ t) = show t
    updPos _ _ (Located p _ : _) = p
    updPos pos _ []              = pos
    test (Located _ t) = f t

-- | Match a 'TagOpen' with the given lowercased name. Returns the
-- block-form flag.
tagOpen :: Text -> Parser Bool
tagOpen name = satisfyTok $ \case
  TagOpen n isBlock | n == name -> Just isBlock
  _                             -> Nothing

tagSym :: Parser ()
tagSym = satisfyTok $ \case
  TagSym -> Just ()
  _      -> Nothing

textTok :: Parser Text
textTok = satisfyTok $ \case
  TextRun t -> Just t
  _         -> Nothing

urlTok :: Parser Text
urlTok = satisfyTok $ \case
  Url t -> Just t
  _     -> Nothing

commaTok :: Parser ()
commaTok = satisfyTok $ \case
  Comma _ -> Just ()
  _       -> Nothing

-- | Like 'commaTok' but yields the comma's full matched source text
-- (including surrounding whitespace). Used in prose so @a, b@ keeps
-- its space rather than collapsing to @a,b@.
commaText :: Parser Text
commaText = satisfyTok $ \case
  Comma t -> Just t
  _       -> Nothing

methodNameTok :: Parser Text
methodNameTok = satisfyTok $ \case
  MethodName n -> Just n
  _            -> Nothing

methodArgsTok :: Parser Text
methodArgsTok = satisfyTok $ \case
  MethodArgs a -> Just a
  _            -> Nothing

hashesTok :: Parser ()
hashesTok = satisfyTok $ \case
  Hashes -> Just ()
  _      -> Nothing

barsTok :: Parser ()
barsTok = satisfyTok $ \case
  Bars -> Just ()
  _    -> Nothing

newlineTok :: Parser ()
newlineTok = satisfyTok $ \case
  Newline -> Just ()
  _       -> Nothing

emptyLinesTok :: Parser ()
emptyLinesTok = satisfyTok $ \case
  EmptyLines -> Just ()
  _          -> Nothing

eolTok :: Parser ()
eolTok = newlineTok <|> emptyLinesTok

skipBlankLines :: Parser ()
skipBlankLines = skipMany (newlineTok <|> emptyLinesTok <|> try wsOnlyLine)
  where
    -- A whitespace-only TextRun (e.g. trailing space on an otherwise
    -- empty line) plus its terminator.
    wsOnlyLine = do
      _ <- satisfyTok $ \case
        TextRun t | T.null (T.strip t) -> Just ()
        _                              -> Nothing
      _ <- newlineTok <|> emptyLinesTok
      pure ()


-- ---------------------------------------------------------------------------
-- Metadata

-- | Tag names recognized as document metadata.
metaFieldNames :: [Text]
metaFieldNames =
  [ "title", "class", "summary", "related", "categories", "redirect"
  , "keyword", "keywords"
  ]

-- | Parse one metadata line: @tag:: value...@ then an eol.
metaField :: Parser (Text, [Text])
metaField = do
  name  <- choice (map (try . tagOpenIf) metaFieldNames)
  vals  <- commaSeparated wordsToText
  _     <- eolTok
  pure (name, vals)
  where
    tagOpenIf n = n <$ tagOpen n

-- | One text value with collapsed whitespace.
wordsToText :: Parser Text
wordsToText = do
  ts <- many1 (textTok <|> urlTok)
  let joined = T.concat ts
      collapsed = T.unwords (T.words joined)
  if T.null collapsed
    then fail "expected non-empty text"
    else pure collapsed

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = p `sepBy1` commaTok

buildMeta :: [(Text, [Text])] -> Meta
buildMeta pairs =
  let normalized = map normalizeKey pairs
      regular    = [ (k, MetaString (T.intercalate ", " vs))
                   | (k, vs) <- normalized
                   , k /= "keyword", k /= "keywords"
                   ]
      keywordVs  = concat [ vs | (k, vs) <- normalized
                               , k == "keyword" || k == "keywords" ]
      kwField    = [ ("keywords", MetaList (map MetaString keywordVs))
                   | not (null keywordVs) ]
  in  Meta (Map.fromList (regular ++ kwField))
  where
    -- @class::@ is a source spelling of the title field.
    normalizeKey ("class", vs) = ("title", vs)
    normalizeKey kv            = kv


-- ---------------------------------------------------------------------------
-- Body

-- | Sequence of blocks separated by blank lines.
bodyBlocks :: Parser [Block]
bodyBlocks = do
  skipBlankLines
  many (block <* skipBlankLines)

-- | One block. Specific tag forms must be tried before paragraphs.
block :: Parser Block
block =
      try structuralHeader
  <|> try methodSectionForm
  <|> try keywordBody
  <|> try noteRange
  <|> try warningRange
  <|> try codeBlock
  <|> try teletypeBlock
  <|> try mathBlock
  <|> try listBlock
  <|> try numberedListBlock
  <|> try treeBlock
  <|> try definitionListBlock
  <|> try tableBlock
  <|> paragraph

structuralHeader :: Parser Block
structuralHeader = choice
  [ try (namedHeader   "description"     "Description")
  , try (namedHeader   "classmethods"    "Class Methods")
  , try (namedHeader   "instancemethods" "Instance Methods")
  , try (namedHeader   "examples"        "Examples")
  , try (titledHeader  "section"         1)
  , try (titledHeader  "subsection"      2)
  ,      titledHeader  "subsubsection"   3
  ]

-- | A name-only section opener like @description::@.
namedHeader :: Text -> Text -> Parser Block
namedHeader tag headingLabel = do
  _ <- tagOpen tag
  putState (sectionContextForTag tag)
  pure (Header 1 nullAttr [Str headingLabel])

-- | Map a level-1 section opener to method context.
sectionContextForTag :: Text -> MethodContext
sectionContextForTag "description"     = InDescription
sectionContextForTag "classmethods"    = InClassMethods
sectionContextForTag "instancemethods" = InInstanceMethods
sectionContextForTag "examples"        = InExamples
sectionContextForTag _                 = NoContext

-- | Title-bearing section opener.
titledHeader :: Text -> Int -> Parser Block
titledHeader tag level = do
  _   <- tagOpen tag
  ils <- trimTrailingWS <$> manyTillEol inline
  case canonicalSectionLabel level ils of
    Just headingLabel -> do
      -- Same context as the bare named-section form.
      putState (sectionContextForTag (canonicalLabelToTag headingLabel))
      pure (Header 1 nullAttr [Str headingLabel])
    Nothing    -> do
      -- Generic top-level sections use generic method context.
      when (level == 1 && tag == "section") (putState InGenericSection)
      pure (Header level ("", [tag], []) ils)
  where
    canonicalLabelToTag "Description"      = "description"
    canonicalLabelToTag "Class Methods"    = "classmethods"
    canonicalLabelToTag "Instance Methods" = "instancemethods"
    canonicalLabelToTag "Examples"         = "examples"
    canonicalLabelToTag _                  = "section"

-- | Canonicalize plain top-level section titles only.
canonicalSectionLabel :: Int -> [Inline] -> Maybe Text
canonicalSectionLabel 1 ils
  | all isPlainTextInline ils =
      case T.toLower (T.filter (/= ' ') (plainTextOf ils)) of
        "description"     -> Just "Description"
        "classmethods"    -> Just "Class Methods"
        "instancemethods" -> Just "Instance Methods"
        "examples"        -> Just "Examples"
        _                 -> Nothing
canonicalSectionLabel _ _ = Nothing

-- | Inline shapes that 'plainTextOf' preserves exactly.
isPlainTextInline :: Inline -> Bool
isPlainTextInline (Str _) = True
isPlainTextInline Space   = True
isPlainTextInline _       = False

plainTextOf :: [Inline] -> Text
plainTextOf = T.concat . map go
  where
    go (Str t) = t
    go Space   = " "
    go _       = ""

-- | Run @p@ until a line terminator (newline or empty lines), an
-- unexpected token, or EOF. The terminator is consumed if present.
manyTillEol :: Parser a -> Parser [a]
manyTillEol p = loop []
  where
    loop acc =
          (eof                   >> pure (reverse acc))
      <|> (try emptyLinesTok     >> pure (reverse acc))
      <|> (try newlineTok        >> pure (reverse acc))
      <|> (do x <- try p; loop (x : acc))
      <|> pure (reverse acc)

paragraph :: Parser Block
paragraph = do
  ils <- trimTrailingWS <$> paragraphInlines
  if null ils
    then fail "empty paragraph"
    else pure (Para ils)

-- | Consume paragraph inlines; source newlines become 'LineBreak'.
paragraphInlines :: Parser [Inline]
paragraphInlines = loop []
  where
    loop acc =
          (eof  >> pure (reverse acc))
      <|> (try emptyLinesTok >> pure (reverse acc))
      <|> (try newlineTok >> loop (LineBreak : acc))
      <|> (do i <- try inline; loop (i : acc))
      <|> pure (reverse acc)

-- | Drop dangling whitespace and line-end markers.
trimTrailingWS :: [Inline] -> [Inline]
trimTrailingWS = reverse . dropWhile isWS . reverse
  where
    isWS Space     = True
    isWS SoftBreak = True
    isWS LineBreak = True
    isWS _         = False

inline :: Parser Inline
inline =
      try inlineEmph
  <|> try inlineStrong
  <|> try inlineSoft
  <|> try inlineTeletype
  <|> try inlineCode
  <|> try inlineMath
  <|> try inlineImage
  <|> try inlineAnchor
  <|> try inlineFootnote
  <|> try inlineLink
  <|> try inlineUrl
  <|> inlineText

-- | @footnote:: body ::@ as an inline note.
inlineFootnote :: Parser Inline
inlineFootnote = do
  _ <- tagOpen "footnote"
  Note <$> blocksUntilTagSym

inlineEmph :: Parser Inline
inlineEmph = wrapInlines "emphasis" Emph

inlineStrong :: Parser Inline
inlineStrong = wrapInlines "strong" Strong

inlineSoft :: Parser Inline
inlineSoft = wrapInlines "soft" Strikeout

inlineTeletype :: Parser Inline
inlineTeletype = do
  _ <- tagOpenInlineForm "teletype"
  body <- manyTill inline tagSym
  pure (Span ("", ["teletype"], []) body)

-- | Tag whose body parses as inlines, wrapped by the given constructor.
wrapInlines :: Text -> ([Inline] -> Inline) -> Parser Inline
wrapInlines name ctor = do
  _ <- tagOpen name
  body <- manyTill inline tagSym
  pure (ctor body)

inlineCode :: Parser Inline
inlineCode = verbatimInlineTag "code" (Code nullAttr)

inlineMath :: Parser Inline
inlineMath = verbatimInlineTag "math" (Math InlineMath)

inlineImage :: Parser Inline
inlineImage = verbatimInlineTag "image" $ \body ->
  let (target, caption, mlink) = splitImageBody body
      attr = case mlink of
        Nothing -> nullAttr
        Just l  -> ("", [], [("link", l)])
      alt = [Str caption | not (T.null caption)]
  in  Image attr alt (target, "")

inlineAnchor :: Parser Inline
inlineAnchor = verbatimInlineTag "anchor" $ \body ->
  RawInline (Format "schelp") ("anchor::" <> body <> "::")

-- | Inline-form verbatim tag: @name::body::@ with a body transform.
verbatimInlineTag :: Text -> (Text -> Inline) -> Parser Inline
verbatimInlineTag name f = do
  _    <- tagOpenInlineForm name
  body <- verbatimBody
  _    <- tagSym
  pure (f body)

inlineLink :: Parser Inline
inlineLink = do
  _    <- tagOpen "link"
  body <- verbatimBody
  _    <- tagSym
  -- Canonicalize at parse time to match writer output.
  let stripped      = T.strip body
      (target0, l0) = splitLinkRecovering stripped
      target        = normalizeLinkTarget (T.strip target0)
      lab           = T.strip l0
      finalLabel
        | shouldOmitLinkLabel target lab = []
        | otherwise                      = textToInlines lab
  pure (Link nullAttr finalLabel (target, ""))

-- | Split a link body, trimming one malformed trailing @#@ when safe.
splitLinkRecovering :: Text -> (Text, Text)
splitLinkRecovering body =
  case splitLinkBody body of
    (_, "") | not ("##" `T.isInfixOf` body)
            , "#"  `T.isSuffixOf` body
            -> splitLinkBody (T.dropEnd 1 body)
    result  -> result

-- | Split @target##label@ or @target#anchor#label@.
splitLinkBody :: Text -> (Text, Text)
splitLinkBody body =
  case T.breakOn "##" body of
    (t, rest) | not (T.null rest) -> (t, T.drop 2 rest)
    _ | T.count "#" body >= 2 ->
        let (tWithHash, lab) = T.breakOnEnd "#" body
        in  (T.dropEnd 1 tWithHash, lab)
    _ -> (body, "")

textToInlines :: Text -> [Inline]
textToInlines t
  | T.null t  = []
  | otherwise = [Str t]

inlineUrl :: Parser Inline
inlineUrl = do
  u <- urlTok
  -- Empty label matches @link::url::@.
  pure (Link nullAttr [] (u, ""))


-- ---------------------------------------------------------------------------
-- Range blocks and verbatim block-form modal tags

-- | @note:: ... ::@ as a 'BlockQuote'.
noteRange :: Parser Block
noteRange = do
  _ <- tagOpen "note"
  BlockQuote <$> blocksUntilTagSym

-- | @warning:: ... ::@ as a warning 'Div'.
warningRange :: Parser Block
warningRange = do
  _ <- tagOpen "warning"
  Div ("", ["warning"], []) <$> blocksUntilTagSym

-- | Parse a flat block sequence terminated by a closing @::@. The
-- closing 'TagSym' is consumed.
blocksUntilTagSym :: Parser [Block]
blocksUntilTagSym = do
  skipBlankLines
  manyTill (block <* skipBlankLines) (try tagSym)

codeBlock :: Parser Block
codeBlock = verbatimBlockTag "code" nullAttr

teletypeBlock :: Parser Block
teletypeBlock = verbatimBlockTag "teletype" ("", ["teletype"], [])

mathBlock :: Parser Block
mathBlock = verbatimBlockTag "math" ("", ["math"], [])

-- | Block-form verbatim tag: @name::\\n body \\n::@ as a 'CodeBlock'.
verbatimBlockTag :: Text -> Attr -> Parser Block
verbatimBlockTag name attr = do
  _    <- tagOpenBlockForm name
  body <- verbatimBlockBody
  _    <- tagSym
  pure (CodeBlock attr body)


-- ---------------------------------------------------------------------------
-- Method sections

-- | Method-body declarations as flat sibling blocks.
methodSectionForm :: Parser Block
methodSectionForm = choice
  [ try methodHeader
  , try argumentHeader
  , try returnsHeader
  , try discussionHeader
  , try privateHeader
  , try copymethodHeader
  ,     classtreeHeader
  ]

-- | @method:: name1, name2 (args)?@.
methodHeader :: Parser Block
methodHeader = do
  _     <- tagOpen "method"
  names <- methodNameTok `sepBy1` commaTok
  margs <- optionMaybe methodArgsTok
  _     <- optional eolTok
  ctx   <- getState
  let nameText = T.intercalate ", " names
      -- METHODARGS includes the parentheses; the writer emits them verbatim.
      kvs = case margs of
        Just a  -> [("args", a)]
        Nothing -> []
      cls = "method" : contextClass "method" ctx
  pure (Header 2 ("", cls, kvs) [Str nameText])

-- | Add method/private/copymethod context class.
contextClass :: Text -> MethodContext -> [Text]
contextClass base ctx = case (base, ctx) of
  ("method",     InClassMethods)    -> ["cmethod"]
  ("method",     InInstanceMethods) -> ["imethod"]
  -- SCDoc only distinguishes cprivate from iprivate.
  ("private",    InClassMethods)    -> ["cprivate"]
  ("private",    _)                 -> ["iprivate"]
  ("copymethod", InClassMethods)    -> ["ccopymethod"]
  ("copymethod", InInstanceMethods) -> ["icopymethod"]
  _                                 -> []

-- | @argument:: name@.
argumentHeader :: Parser Block
argumentHeader = do
  _   <- tagOpen "argument"
  ils <- trimTrailingWS <$> manyTillEol inline
  pure (Header 3 ("", ["argument"], []) ils)

-- | @returns::@; body content follows as sibling blocks.
returnsHeader :: Parser Block
returnsHeader = do
  _ <- tagOpen "returns"
  pure (Header 3 ("", ["returns"], []) [])

-- | @discussion::@. Same shape as @returns::@.
discussionHeader :: Parser Block
discussionHeader = do
  _ <- tagOpen "discussion"
  pure (Header 3 ("", ["discussion"], []) [])

-- | @private:: name1, name2@.
privateHeader :: Parser Block
privateHeader = do
  _   <- tagOpen "private"
  ils <- trimTrailingWS <$> manyTillEol inline
  ctx <- getState
  pure (Header 2 ("", "private" : contextClass "private" ctx, []) ils)

-- | @copymethod:: Class method@ or @copymethod:: Class.method@.
copymethodHeader :: Parser Block
copymethodHeader = do
  _   <- tagOpen "copymethod"
  ils <- trimTrailingWS <$> manyTillEol inline
  ctx <- getState
  pure (Header 2 ("", "copymethod" : contextClass "copymethod" ctx, []) ils)

-- | @classtree:: ClassName@.
classtreeHeader :: Parser Block
classtreeHeader = do
  _   <- tagOpen "classtree"
  ils <- trimTrailingWS <$> manyTillEol inline
  pure (Header 2 ("", ["classtree"], []) ils)

-- | Body-level @keyword:: foo, bar@, lifted into 'Meta' later.
keywordBody :: Parser Block
keywordBody = do
  _      <- tagOpen "keyword"
  values <- commaSeparated wordsToText
  _      <- optional eolTok
  pure (RawBlock (Format "schelp") ("keyword:: " <> T.intercalate ", " values))


-- ---------------------------------------------------------------------------
-- Lists, trees, definition lists, and tables

-- | @list:: ## a ## b ::@ -> 'BulletList'.
listBlock :: Parser Block
listBlock = hashItemsTag "list" BulletList

-- | @numberedlist:: …@ -> 'OrderedList'.
numberedListBlock :: Parser Block
numberedListBlock = hashItemsTag "numberedlist"
                      (OrderedList (1, DefaultStyle, DefaultDelim))

-- | @tree:: ...@ as a @tree@ 'Div' containing a 'BulletList'.
treeBlock :: Parser Block
treeBlock = hashItemsTag "tree" (\items -> Div ("", ["tree"], []) [BulletList items])

-- | @name:: ## item ## item ::@ with a wrap function for the parsed items.
hashItemsTag :: Text -> ([[Block]] -> Block) -> Parser Block
hashItemsTag name wrap = do
  _     <- tagOpen name
  items <- hashItems
  _     <- tagSym
  pure (wrap items)

-- | A sequence of one or more @## body@ items, separated by blank lines.
hashItems :: Parser [[Block]]
hashItems = do
  skipBlankLines
  many1 hashItem

hashItem :: Parser [Block]
hashItem = do
  _ <- hashesTok
  skipBlankLines
  manyTill (block <* skipBlankLines) endOfItem
  where
    endOfItem = lookAhead $ try (void hashesTok <|> tagSym)

-- | @definitionlist::@ with @##@ terms and @||@ definitions.
definitionListBlock :: Parser Block
definitionListBlock = do
  _ <- tagOpen "definitionlist"
  skipBlankLines
  rows <- many1 defListRow
  _ <- tagSym
  pure (DefinitionList rows)

defListRow :: Parser ([Inline], [[Block]])
defListRow = do
  termSets <- many1 defTermLine
  defs     <- many (try defListDefinition)
  -- Empty @||@ bodies represent no definition.
  let defs' = filter (not . null) defs
      term  = case termSets of
        [single] -> single
        many'    -> groupedTerms many'
  pure (term, defs')
  where
    groupedTerms tss =
      concat $
        zipWith (\i ts -> (if i == 0 then [] else [LineBreak])
                       <> [Span ("", ["scdoc-defterm"], []) ts])
                [0 :: Int ..] tss

-- | One @## term@ line, including attached @keyword::@ annotations.
defTermLine :: Parser [Inline]
defTermLine = do
  _        <- hashesTok
  termBody <- trimTrailingWS <$> termInlines
  skipBlankLines
  kws      <- many (try (keywordAnnotation <* skipBlankLines))
  pure (termBody <> concatMap kwInlines kws)
  where
    kwInlines name = [LineBreak, RawInline (Format "schelp") name]

-- | A @keyword:: foo, bar@ annotation preserved inside a term.
keywordAnnotation :: Parser Text
keywordAnnotation = do
  _      <- tagOpen "keyword"
  values <- commaSeparated wordsToText
  _      <- optional eolTok
  pure ("keyword:: " <> T.intercalate ", " values)

termInlines :: Parser [Inline]
termInlines = loop []
  where
    loop acc =
          (eof                            >> pure (reverse acc))
      <|> (try emptyLinesTok              >> pure (reverse acc))
      <|> (lookAhead (try hashesTok)      >> pure (reverse acc))
      <|> (lookAhead (try barsTok)        >> pure (reverse acc))
      <|> (lookAhead (try tagSym)         >> pure (reverse acc))
      -- Multi-line terms collapse to one-line text on write.
      <|> (try newlineTok                 >> loop (Space : acc))
      <|> (do i <- try inline; loop (i : acc))
      -- Preserve unknown tag openers when syntax is shown as text.
      <|> (do i <- literalTagOpen; loop (i : acc))
      <|> pure (reverse acc)

-- | Emit any 'TagOpen' token as raw SCDoc text.
literalTagOpen :: Parser Inline
literalTagOpen = satisfyTok $ \case
  TagOpen name _ -> Just (RawInline (Format "schelp") (name <> "::"))
  _              -> Nothing

-- | One @||@ definition body.
defListDefinition :: Parser [Block]
defListDefinition = do
  _ <- barsTok
  skipBlankLines
  manyTill (block <* skipBlankLines) endOfDef
  where
    endOfDef = lookAhead $ try $
          void barsTok
      <|> void hashesTok
      <|> tagSym

-- | @table:: ## cell || cell ::@ -> 'Table'. The first row becomes the
-- header; subsequent rows the body.
tableBlock :: Parser Block
tableBlock = do
  _ <- tagOpen "table"
  skipBlankLines
  rows <- many1 tableRowP
  _ <- tagSym
  pure (mkTable rows)

tableRowP :: Parser [[Block]]
tableRowP = do
  _     <- hashesTok
  cells <- tableCell `sepBy1` barsTok
  _     <- optional eolTok
  skipBlankLines
  pure cells

-- | Table cell body up to the next separator or close.
tableCell :: Parser [Block]
tableCell = do
  skipBlankLines
  manyTill (block <* skipBlankLines) endOfCell
  where
    endOfCell = lookAhead $ try $
          void barsTok
      <|> void hashesTok
      <|> tagSym

mkTable :: [[[Block]]] -> Block
mkTable []       =
  Table nullAttr (Caption Nothing []) []
    (TableHead nullAttr [])
    []
    (TableFoot nullAttr [])
mkTable (h:body) =
  Table nullAttr (Caption Nothing []) []
    (TableHead nullAttr [mkRow h])
    [TableBody nullAttr (RowHeadColumns 0) [] (map mkRow body)]
    (TableFoot nullAttr [])
  where
    mkRow cells = Row nullAttr (map mkCell cells)
    mkCell bs   = Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) bs


-- ---------------------------------------------------------------------------
-- Tag-form discrimination

tagOpenBlockForm :: Text -> Parser ()
tagOpenBlockForm name = try $ do
  isBlock <- tagOpen name
  unless isBlock (fail (T.unpack name <> ": not in block form"))

tagOpenInlineForm :: Text -> Parser ()
tagOpenInlineForm name = try $ do
  isBlock <- tagOpen name
  when isBlock (fail (T.unpack name <> ": not in inline form"))


-- | Consume verbatim body tokens before the closing 'TagSym'.
verbatimBody :: Parser Text
verbatimBody = do
  ts <- many (textTok <|> urlTok)
  pure (T.concat ts)

-- | Block-form bodies use the same token shape as inline verbatim bodies.
verbatimBlockBody :: Parser Text
verbatimBlockBody = verbatimBody


-- | Split an @image::@ body into target, caption, and optional link.
splitImageBody :: Text -> (Text, Text, Maybe Text)
splitImageBody body =
  case T.breakOn "##" body of
    (target, rest) | not (T.null rest) ->
      (target, "", Just (T.drop 2 rest))
    _ | T.count "#" body >= 2 ->
        let (target, rest0) = T.breakOn "#" body
            rest1           = T.drop 1 rest0
            (caption, lnk)  = T.breakOnEnd "#" rest1
        in  (target, T.dropEnd 1 caption, Just lnk)
    _ | T.count "#" body == 1 ->
        let (target, rest0) = T.breakOn "#" body
        in  (target, T.drop 1 rest0, Nothing)
    _ -> (body, "", Nothing)

inlineText :: Parser Inline
inlineText = do
  t <- textTok <|> commaText
  pure $ case t of
    " " -> Space
    _   -> Str t
