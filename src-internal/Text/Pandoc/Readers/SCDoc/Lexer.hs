{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tokenizer for SCDoc input.
module Text.Pandoc.Readers.SCDoc.Lexer
  ( Tok (..)
  , Located (..)
  , LexerError
  , LexerState (..)
  , tokenize
  ) where

import           Control.Monad (when)
import           Data.Char     (isAsciiLower, isAsciiUpper, toLower)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Text.Parsec   hiding (tokens)
import           Text.Parsec.Text ()


-- | A single SCDoc token.
data Tok
  = TagOpen   !Text !Bool
  -- ^ A @name::@ opener; 'True' means block-modal form.
  | TagSym
  -- ^ @::@ closer for an inline modal, range, or block-modal tag.
  | Bars
  -- ^ @||@ separator inside a table row or a definition-list row.
  | Hashes
  -- ^ @##@ separator that starts a list item, table row, or definition term.
  | Comma     !Text
  -- ^ Comma plus leading whitespace; trailing whitespace stays separate.
  | Newline
  -- ^ Single line terminator.
  | EmptyLines
  -- ^ Two or more consecutive line terminators (paragraph break).
  | TextRun   !Text
  -- ^ A run of plain text. Whitespace runs are normalized to a single space
  -- per the flex lexer; word runs are preserved verbatim.
  | Url       !Text
  -- ^ An inline URL like @http:\/\/...@ or @file:\/\/...@.
  | MethodName !Text
  -- ^ A method identifier inside the method state.
  | MethodArgs !Text
  -- ^ A parenthesized argument list inside the method state, including the
  -- enclosing parentheses.
  | BadMethodName !Text
  -- ^ A character inside the method state that is not a valid method-name
  -- continuation.
  deriving (Eq, Show)


-- | A token paired with its source position.
data Located a = Located !SourcePos !a
  deriving (Eq, Show)


type LexerError = ParseError


-- | The lexer's current mode. Mirrors the flex start conditions.
data LexerState
  = Initial
  -- ^ Default state — looking for tag openers, text runs, and structural
  -- punctuation.
  | Verbatim
  -- ^ Inside an inline modal tag body (@link::@, @code::@, @emphasis::@,
  -- ...). Whitespace collapses, @\\::@ escapes, the next @[ \\t\\n\\r]*::@
  -- closes the tag.
  | Verbatim2
  -- ^ Inside a block modal tag body (@code::\\n@, @teletype::\\n@,
  -- @math::\\n@). Newlines are literal, @\\n[ \\t\\n\\r]*::@ closes the
  -- tag.
  | InMethod !LexerState
  -- ^ Inside a @method::@ name\/arg list. Carries the state to restore when
  -- a newline is consumed.
  deriving (Eq, Show)


type Lex = Parsec Text LexerState


-- | Tokenize an SCDoc input. The 'FilePath' is for error messages.
tokenize :: FilePath -> Text -> Either LexerError [Located Tok]
tokenize source input =
  runParser (catMaybes <$> many positionedToken <* eof) Initial source
    (T.filter (/= '\r') input)
  where
    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr (\m acc -> maybe acc (: acc) m) []


positionedToken :: Lex (Maybe (Located Tok))
positionedToken = do
  pos <- getPosition
  st  <- getState
  mt  <- case st of
    Initial    -> initialTok
    Verbatim   -> verbatimTok
    Verbatim2  -> verbatim2Tok
    InMethod _ -> methodTok
  pure (Located pos <$> mt)


-- ---------------------------------------------------------------------------
-- INITIAL state

initialTok :: Lex (Maybe Tok)
initialTok =
      Just <$> try blockModalTagOpener
  <|> Just <$> try plainTagOpener
  <|> Just <$> try tagSym
  <|> Just <$> try bars
  <|> Just <$> try hashes
  <|> Just <$> try comma
  <|> Just <$> try emptyLines
  <|> Just <$> try newline'
  <|> Just <$> try url
  <|> Just <$> try escapedDelim
  <|> Just <$> try whitespaceRun
  <|> Just <$> initialTextRun


-- ---------------------------------------------------------------------------
-- VERBATIM state (inside inline modal tag body)

verbatimTok :: Lex (Maybe Tok)
verbatimTok =
      Just <$> try tagSym
  <|> Just <$> try escapedDelim
  <|> Just <$> try verbatimNewlines
  <|> Just <$> try whitespaceRun
  <|> Just <$> nonSpecialRun


-- | @\\n+@ inside verbatim collapses to a single space, per the flex lexer.
verbatimNewlines :: Lex Tok
verbatimNewlines = do
  _ <- many1 (char '\n')
  pure (TextRun " ")

-- | One char plus a run of non-special chars. Shared by both verbatim states.
nonSpecialRun :: Lex Tok
nonSpecialRun = do
  c    <- anyChar
  rest <- many (noneOf ":\\\n\r\t ")
  pure (TextRun (T.pack (c : rest)))


-- ---------------------------------------------------------------------------
-- VERBATIM2 state (inside block modal tag body)

verbatim2Tok :: Lex (Maybe Tok)
verbatim2Tok =
      Just <$> try verbatim2Close
  <|> Just <$> try verbatim2EscapedClose
  <|> Just <$> try verbatim2Newline
  <|> Just <$> try escapedBarsOrHashes
  <|> Just <$> try whitespaceRun
  <|> Just <$> nonSpecialRun

-- | @\\n[ \\t\\n\\r]*::@ closes a verbatim2 block.
verbatim2Close :: Lex Tok
verbatim2Close = do
  _ <- char '\n'
  skipMany (oneOf " \t\n\r")
  _ <- string "::"
  putState Initial
  pure TagSym

-- | A @\\n[ \\t]*\\::@ escape yields literal @\\n::@.
verbatim2EscapedClose :: Lex Tok
verbatim2EscapedClose = do
  _ <- char '\n'
  skipMany (oneOf " \t")
  _ <- string "\\::"
  pure (TextRun "\n::")

verbatim2Newline :: Lex Tok
verbatim2Newline = do
  _ <- char '\n'
  pure (TextRun "\n")


-- ---------------------------------------------------------------------------
-- METHOD state

methodTok :: Lex (Maybe Tok)
methodTok =
      Just <$> try methodArgs
  <|> Just <$> try methodName
  <|> Just <$> try comma
  <|> Nothing <$ try (many1 (oneOf " \r\t"))
  <|> Just <$> try methodNewline
  <|> Just <$> badMethodChar

methodName :: Lex Tok
methodName = do
  first <- letterStart <|> opChar
  case first of
    Left  c -> do
      rest <- many (oneOf nameCont)
      pure (MethodName (T.pack (c : rest)))
    Right c -> do
      rest <- many (oneOf opCharSet)
      pure (MethodName (T.pack (c : rest)))
  where
    letterStart = do
      c <- satisfy isAsciiLower
      pure (Left c)
    opChar = do
      c <- oneOf opCharSet
      pure (Right c)
    nameCont   = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
    opCharSet  = "-<>@|&%*+/!?="

methodArgs :: Lex Tok
methodArgs = do
  _    <- char '('
  body <- many (noneOf "()")
  _    <- char ')'
  pure (MethodArgs (T.pack ("(" <> body <> ")")))

methodNewline :: Lex Tok
methodNewline = do
  _ <- char '\n'
  st <- getState
  case st of
    InMethod caller -> putState caller
    _               -> pure ()
  pure Newline

badMethodChar :: Lex Tok
badMethodChar = BadMethodName . T.singleton <$> anyChar


-- ---------------------------------------------------------------------------
-- Shared rules

-- | Match @[ \\t\\n\\r]*::@ and return to 'Initial'.
tagSym :: Lex Tok
tagSym = do
  skipMany (oneOf " \t\n\r")
  _ <- string "::"
  putState Initial
  pure TagSym

bars :: Lex Tok
bars = do
  skipMany (oneOf " \t")
  _ <- string "||"
  skipMany (oneOf " \t\n\r")
  pure Bars

hashes :: Lex Tok
hashes = do
  skipMany (oneOf " \t")
  _ <- string "##"
  skipMany (oneOf " \t\n\r")
  pure Hashes

comma :: Lex Tok
comma = do
  -- Keep leading whitespace; leave trailing whitespace as text.
  pre <- many (oneOf " \t")
  _   <- char ','
  pure (Comma (T.pack (pre <> ",")))

emptyLines :: Lex Tok
emptyLines = do
  _ <- char '\n'
  -- Roll back partial indentation unless another newline follows.
  _ <- many1 (try (skipMany (oneOf " \t\r") >> char '\n'))
  pure EmptyLines

newline' :: Lex Tok
newline' = char '\n' >> pure Newline

escapedDelim :: Lex Tok
escapedDelim = do
  _ <- char '\\'
  esc <- string "::" <|> string "||" <|> string "##"
  pure (TextRun (T.pack esc))

-- | @\\||@ and @\\##@ unescape in any state, including verbatim2 — these
-- two escape rules are unscoped in the flex lexer.
escapedBarsOrHashes :: Lex Tok
escapedBarsOrHashes = do
  _ <- char '\\'
  esc <- string "||" <|> string "##"
  pure (TextRun (T.pack esc))

whitespaceRun :: Lex Tok
whitespaceRun = do
  _ <- many1 (oneOf " \t")
  pure (TextRun " ")

-- | Plain-text run in 'Initial', split at letter/punctuation boundaries.
initialTextRun :: Lex Tok
initialTextRun = do
  c    <- noneOf "\r"
  rest <- continuation c
  pure (TextRun (T.pack (c : rest)))
  where
    continuation :: Char -> Lex [Char]
    continuation ch
      | isAsciiLetterC ch = many (satisfy isAsciiLetterC)
      | isPunctDigit ch   = many (satisfy isPunctDigit)
      | otherwise         = pure ""

    isAsciiLetterC c = isAsciiLower c || isAsciiUpper c
    isPunctDigit   c =
      c `elem` (".!?(){}[]'\"" :: String)
        || (c >= '0' && c <= '9')

url :: Lex Tok
url = do
  scheme <- many1 (satisfy isAsciiLetter)
  _      <- string "://"
  rest   <- many1 (noneOf " \t\n\r:,")
  pure (Url (T.pack (scheme <> "://" <> rest)))
  where
    isAsciiLetter c = isAsciiLower c || isAsciiUpper c


-- ---------------------------------------------------------------------------
-- Tag openers

-- | Single-line and section tags.
-- Entries are @(name, eatsTrailingNewlines, ifMethodEnters)@.
plainTagSpecs :: [(Text, Bool, Bool)]
plainTagSpecs =
  [ ("class",          False, False)
  , ("title",          False, False)
  , ("summary",        False, False)
  , ("related",        False, False)
  , ("categories",     False, False)
  , ("redirect",       False, False)
  , ("classtree",      False, False)
  , ("keyword",        False, False)
  , ("private",        False, False)
  , ("section",        False, False)
  , ("subsection",     False, False)
  , ("subsubsection",  False, False)
  , ("copymethod",     False, False)
  , ("argument",       False, False)
  , ("method",         False, True )
  , ("description",    True,  False)
  , ("classmethods",   True,  False)
  , ("instancemethods",True,  False)
  , ("examples",       True,  False)
  , ("returns",        True,  False)
  , ("discussion",     True,  False)
  , ("list",           True,  False)
  , ("tree",           True,  False)
  , ("numberedlist",   True,  False)
  , ("definitionlist", True,  False)
  , ("table",          True,  False)
  , ("footnote",       True,  False)
  , ("warning",        True,  False)
  , ("note",           True,  False)
  -- inline modal tags — enter Verbatim
  , ("link",           False, False)
  , ("anchor",         False, False)
  , ("image",          False, False)
  , ("code",           False, False)
  , ("teletype",       False, False)
  , ("math",           False, False)
  , ("soft",           True,  False)
  , ("strong",         True,  False)
  , ("emphasis",       True,  False)
  ]

-- | Inline modal tags that switch into 'Verbatim'.
inlineModalTagNames :: [Text]
inlineModalTagNames =
  ["link", "anchor", "image", "code", "teletype", "math",
   "soft", "strong", "emphasis"]

-- | Tags that are line-form at column 1 and literal mid-line.
ambiguousLineFormTagNames :: [Text]
ambiguousLineFormTagNames = ["footnote", "keyword"]

-- | Names that can appear in the block-modal form
-- (@[ \\t]*name::[ \\t]*\\n+@).
blockModalTagNames :: [Text]
blockModalTagNames = ["code", "teletype", "math"]

plainTagOpener :: Lex Tok
plainTagOpener = choice (map (try . openerFor) plainTagSpecs)
  where
    openerFor (name, eatNl, methodEnter) = do
      pos <- getPosition
      let isInlineModal  = name `elem` inlineModalTagNames
          atSOL          = sourceColumn pos == 1
          ambiguous      = name `elem` ambiguousLineFormTagNames
          -- Preserve mid-line spacing for ambiguous line-form tags.
          shouldEatWs    = not isInlineModal && (atSOL || not ambiguous)
      when shouldEatWs (skipMany (oneOf " \t"))
      _ <- asciiCI name
      _ <- string "::"
      if eatNl
        then skipMany (oneOf " \t\n\r")
        else skipMany (oneOf " \t")
      if methodEnter
        then do
          caller <- getState
          putState (InMethod caller)
        else when isInlineModal (putState Verbatim)
      pure (TagOpen name False)

-- | Block form for code\/teletype\/math; tried before plain openers.
blockModalTagOpener :: Lex Tok
blockModalTagOpener = choice (map (try . openerFor) blockModalTagNames)
  where
    openerFor name = do
      skipMany (oneOf " \t")
      _ <- asciiCI name
      _ <- string "::"
      skipMany (oneOf " \t")
      _ <- many1 (char '\n')
      putState Verbatim2
      pure (TagOpen name True)


asciiCI :: Text -> Lex Text
asciiCI s = do
  let target = T.unpack s
  mapM_ (\c -> satisfy (\x -> toLower x == toLower c)) target
  pure s
