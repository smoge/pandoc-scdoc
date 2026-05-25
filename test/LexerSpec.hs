{-# LANGUAGE OverloadedStrings #-}

module LexerSpec (spec) where

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           System.Directory                (doesDirectoryExist)
import           System.Environment              (lookupEnv)
import           System.FilePath                 ((</>))
import           Test.Hspec

import           Text.Pandoc.Readers.SCDoc.Lexer (Located (..), Tok (..),
                                                  tokenize)


-- | Tokenize and strip source positions, for terse equality assertions.
tokens :: Text -> [Tok]
tokens t = case tokenize "<test>" t of
  Left  err -> error (show err)
  Right ts  -> map (\(Located _ a) -> a) ts


helpSourceEnv :: String
helpSourceEnv = "SCDOC_HELPSOURCE"


helpSourceRootFromEnv :: IO (Maybe FilePath)
helpSourceRootFromEnv = do
  root <- lookupEnv helpSourceEnv
  pure $ case root of
    Just path | not (null path) -> Just path
    _                           -> Nothing


spec :: Spec
spec = do
  describe "tag openers" $ do

    it "title:: foo emits one TagOpen followed by text" $
      tokens "title:: foo\n"
        `shouldBe` [TagOpen "title" False, TextRun "foo", Newline]

    it "class:: SinOsc is recognized (writer-emitted is title::, but real corpus uses class::)" $
      tokens "class:: SinOsc\n"
        `shouldBe` [TagOpen "class" False, TextRun "SinOsc", Newline]

    it "tag opener is case-insensitive" $
      tokens "Title:: x\n"
        `shouldBe` [TagOpen "title" False, TextRun "x", Newline]

    it "indented keyword:: at column 1 still consumes its leading ws" $
      -- Leading spaces belong to a line-form opener.
      tokens "   keyword:: foo\n"
        `shouldBe` [TagOpen "keyword" False, TextRun "foo", Newline]

    it "mid-line keyword:: preserves the preceding space" $
      -- Mid-line leading space is real text.
      tokens "term keyword:: foo\n"
        `shouldBe`
          [ TextRun "term", TextRun " "
          , TagOpen "keyword" False, TextRun "foo", Newline
          ]

    it "mid-line footnote:: preserves the preceding space" $
      tokens "see footnote::body::"
        `shouldBe`
          [ TextRun "see", TextRun " "
          , TagOpen "footnote" False, TextRun "body", TagSym
          ]

    it "indented footnote:: at column 1 consumes its leading ws" $
      tokens "  footnote::body::"
        `shouldBe` [TagOpen "footnote" False, TextRun "body", TagSym]

    it "categories:: a, b emits Comma with leading ws; trailing ws is its own TextRun" $
      -- Comma keeps leading whitespace but leaves trailing whitespace as text.
      tokens "categories:: a, b\n"
        `shouldBe`
          [TagOpen "categories" False, TextRun "a", Comma ",",
           TextRun " ", TextRun "b", Newline]

    it "Comma keeps leading whitespace but not trailing" $
      tokens "a , b\n"
        `shouldBe`
          [TextRun "a", Comma " ,", TextRun " ", TextRun "b", Newline]

    it "Comma with no surrounding whitespace is text \",\"" $
      tokens "a,b\n"
        `shouldBe` [TextRun "a", Comma ",", TextRun "b", Newline]

    it "leading whitespace before a block tag is consumed by the opener" $
      tokens "   description::\n"
        `shouldBe` [TagOpen "description" False]


  describe "code blocks" $ do

    it "code::\\n enters Verbatim2 and \\n:: closes it" $
      tokens "code::\nfoo\n::"
        `shouldBe` [TagOpen "code" True, TextRun "foo", TagSym]

    it "a leading-\\n \\:: line inside verbatim2 is a literal ::" $
      -- Escapes only a line that would otherwise close verbatim2.
      tokens "code::\nfoo\n\\::\nbar\n::"
        `shouldBe`
          [ TagOpen "code" True
          , TextRun "foo"
          , TextRun "\n::"
          , TextRun "\n"
          , TextRun "bar"
          , TagSym
          ]

    it "preserves multi-line content with internal whitespace" $
      tokens "code::\nfoo bar\nbaz\n::"
        `shouldBe`
          [ TagOpen "code" True
          , TextRun "foo", TextRun " ", TextRun "bar"
          , TextRun "\n"
          , TextRun "baz"
          , TagSym
          ]

    it "inline code:: enters Verbatim, not Verbatim2" $
      tokens "code::x::"
        `shouldBe` [TagOpen "code" False, TextRun "x", TagSym]


  describe "inline modal tags" $ do

    it "emphasis::word:: round-trips through the lexer" $
      tokens "emphasis::word::"
        `shouldBe` [TagOpen "emphasis" False, TextRun "word", TagSym]

    it "link::Classes/SinOsc##Label:: keeps ## as text inside verbatim" $
      tokens "link::Classes/SinOsc##Label::"
        `shouldBe`
          [TagOpen "link" False, TextRun "Classes/SinOsc##Label", TagSym]


  describe "structural punctuation" $ do

    it "## item produces Hashes followed by text" $
      tokens "## one\n"
        `shouldBe` [Hashes, TextRun "one", Newline]

    it "|| separator produces Bars" $
      tokens "## a || b\n"
        `shouldBe`
          [ Hashes
          , TextRun "a"
          , Bars
          , TextRun "b"
          , Newline
          ]


  describe "text-run grouping" $ do

    it "splits letter runs from punct so a leading ( doesn't hide a tag" $
      -- Letter/punctuation split lets tag openers start after punctuation.
      tokens "(link::body::)"
        `shouldBe`
          [ TextRun "("
          , TagOpen "link" False
          , TextRun "body"
          , TagSym
          , TextRun ")"
          ]

    it "groups consecutive letters into one TextRun" $
      tokens "abc def\n"
        `shouldBe` [TextRun "abc", TextRun " ", TextRun "def", Newline]

    it "groups consecutive punct/digits into one TextRun" $
      tokens "...123!"
        `shouldBe` [TextRun "...123!"]

    it "letters and punct/digits do not mix in a single run" $
      tokens "abc123def" `shouldBe`
        [TextRun "abc", TextRun "123", TextRun "def"]


  describe "newlines and paragraph breaks" $ do

    it "single \\n is Newline" $
      tokens "a\nb"
        `shouldBe` [TextRun "a", Newline, TextRun "b"]

    it "two consecutive \\n form EmptyLines" $
      tokens "a\n\nb"
        `shouldBe` [TextRun "a", EmptyLines, TextRun "b"]

    it "CRLF line endings are normalized to LF (parity with flex <*>\\r)" $
      tokens "a\r\nb"
        `shouldBe` [TextRun "a", Newline, TextRun "b"]

    it "trailing bare CR does not cause a parse failure" $
      -- CR stripping happens before positioned lexing.
      tokens "title:: Foo\n\r\r"
        `shouldBe` [TagOpen "title" False, TextRun "Foo", Newline]

    it "EmptyLines matches \\n\\n followed by a tab-indented line" $
      -- Keep the paragraph break before an indented continuation.
      tokens "a\n\n\tb"
        `shouldBe`
          [TextRun "a", EmptyLines, TextRun " ", TextRun "b"]


  describe "escapes" $ do

    it "\\:: outside verbatim is a literal ::" $
      tokens "foo \\:: bar"
        `shouldBe`
          [ TextRun "foo"
          , TextRun " "
          , TextRun "::"
          , TextRun " "
          , TextRun "bar"
          ]


  describe "URLs" $ do

    it "http:// URL emits a Url token" $
      tokens "see https://example.com end"
        `shouldBe`
          [ TextRun "see"
          , TextRun " "
          , Url "https://example.com"
          , TextRun " "
          , TextRun "end"
          ]


  describe "method state" $ do

    it "method:: ar enters method state, captures METHODNAME, NEWLINE exits" $
      tokens "method:: ar\nbody"
        `shouldBe`
          [ TagOpen "method" False
          , MethodName "ar"
          , Newline
          , TextRun "body"
          ]

    it "method:: ar, kr captures comma-separated names" $
      -- Method-state whitespace after comma is skipped.
      tokens "method:: ar, kr\n"
        `shouldBe`
          [ TagOpen "method" False
          , MethodName "ar"
          , Comma ","
          , MethodName "kr"
          , Newline
          ]

    it "method:: ar(freq=440) captures METHODARGS" $
      tokens "method:: ar(freq=440)\n"
        `shouldBe`
          [ TagOpen "method" False
          , MethodName "ar"
          , MethodArgs "(freq=440)"
          , Newline
          ]

    it "method:: == captures an operator-style method name" $
      tokens "method:: ==\n"
        `shouldBe`
          [ TagOpen "method" False
          , MethodName "=="
          , Newline
          ]


  describe "real-corpus samples" $ do

    it "tokenizes a SinOsc.schelp argument section without error" $ do
      let sample = T.unlines
            [ "argument::freq"
            , "Frequency in Hertz."
            , "Sampled at audio-rate."
            ]
      tokenize "<sample>" sample `shouldSatisfy` isRight

    mCorpusRoot <- runIO helpSourceRootFromEnv
    corpusExists <- runIO $
      maybe (pure False) doesDirectoryExist mCorpusRoot

    case mCorpusRoot of
      Nothing -> pure ()
      Just corpusRoot | not corpusExists ->
        it "requires an existing HelpSource checkout" $
          expectationFailure ("corpus root not found at " <> corpusRoot)
      Just corpusRoot -> do
        it "tokenizes SinOsc.schelp from HelpSource without error" $ do
          let path = corpusRoot </> "Classes/SinOsc.schelp"
          input <- TIO.readFile path
          tokenize path input `shouldSatisfy` isRight

        -- Representative corpus slice.
        it "tokenizes a handful of HelpSource files without error" $ do
          let paths =
                [ corpusRoot </> "Classes/Char.schelp"
                  -- verbatim backslash
                , corpusRoot </> "Classes/CallOnce.schelp"
                  -- single ':' inside verbatim
                , corpusRoot </> "Classes/Document.schelp"
                  -- long method sections
                , corpusRoot </> "Classes/SinOsc.schelp"
                ]
          results <- mapM (\p -> do
                             input <- TIO.readFile p
                             pure (p, tokenize p input)) paths
          let failures = [(p, e) | (p, Left e) <- results]
          failures `shouldBe` []


isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
