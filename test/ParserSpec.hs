{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the SCDoc reader's token-based parser.
module ParserSpec (spec) where

import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import           Test.Hspec

import qualified Text.Pandoc               as Pandoc
import           Text.Pandoc.Definition
import           Text.Pandoc.Options       (def)
import           Text.Pandoc.Readers.SCDoc (readSCDoc)


-- | Read input through the reader. Fails the test on parse error.
readDoc :: Text -> IO Pandoc
readDoc input = do
  r <- Pandoc.runIO (readSCDoc def input)
  case r of
    Left  e  -> error (show e)
    Right pd -> pure pd

spec :: Spec
spec = do

  -- -------------------------------------------------------------------------
  describe "Metadata" $ do

    it "lifts title:: into Meta" $ do
      Pandoc meta _ <- readDoc "title:: MySynth\n"
      Map.lookup "title" (unMeta meta) `shouldBe` Just (MetaString "MySynth")

    it "lifts class:: into the title field (real-corpus convention)" $ do
      Pandoc meta _ <- readDoc "class:: SinOsc\n"
      Map.lookup "title" (unMeta meta) `shouldBe` Just (MetaString "SinOsc")

    it "class:: leaves no separate 'class' key in Meta" $ do
      -- @class::@ is a source spelling of the title field.
      Pandoc meta _ <- readDoc "class:: SinOsc\n"
      Map.lookup "class" (unMeta meta) `shouldBe` Nothing

    it "title:: wins when followed by class:: (last-in-doc-order wins)" $ do
      -- Duplicate metadata keeps the later normalized key.
      Pandoc meta _ <- readDoc "title:: Real\nclass:: Override\n"
      Map.lookup "title" (unMeta meta)
        `shouldBe` Just (MetaString "Override")

    it "class:: followed by title:: keeps the explicit title" $ do
      Pandoc meta _ <- readDoc "class:: Auto\ntitle:: Explicit\n"
      Map.lookup "title" (unMeta meta)
        `shouldBe` Just (MetaString "Explicit")

    it "lifts summary::" $ do
      Pandoc meta _ <- readDoc "summary:: A resonant filter\n"
      Map.lookup "summary" (unMeta meta)
        `shouldBe` Just (MetaString "A resonant filter")

    it "lifts categories:: as a single comma-joined string" $ do
      Pandoc meta _ <- readDoc "categories:: UGens, Filters\n"
      Map.lookup "categories" (unMeta meta)
        `shouldBe` Just (MetaString "UGens, Filters")

    it "lifts keyword:: into a MetaList" $ do
      Pandoc meta _ <- readDoc "keyword:: filter\n"
      Map.lookup "keywords" (unMeta meta)
        `shouldBe` Just (MetaList [MetaString "filter"])

    it "multiple metadata lines populate distinct fields" $ do
      Pandoc meta _ <- readDoc "title:: Foo\nsummary:: Bar\n"
      Map.lookup "title"   (unMeta meta) `shouldBe` Just (MetaString "Foo")
      Map.lookup "summary" (unMeta meta) `shouldBe` Just (MetaString "Bar")


  -- -------------------------------------------------------------------------
  describe "Body — paragraphs and simple inlines" $ do

    it "single paragraph of plain text" $ do
      Pandoc _ blocks <- readDoc "hello world\n"
      blocks `shouldBe` [Para [Str "hello", Space, Str "world"]]

    it "paragraph break separates two paragraphs" $ do
      Pandoc _ blocks <- readDoc "first\n\nsecond\n"
      blocks `shouldBe` [Para [Str "first"], Para [Str "second"]]

    -- In-paragraph source newlines become 'LineBreak'.
    it "multi-line paragraph emits LineBreak between source lines" $ do
      Pandoc _ blocks <- readDoc "Line one\nLine two\nLine three\n"
      blocks `shouldBe`
        [Para [ Str "Line", Space, Str "one"
              , LineBreak
              , Str "Line", Space, Str "two"
              , LineBreak
              , Str "Line", Space, Str "three"
              ]]

    it "emphasis:: opens and :: closes" $ do
      Pandoc _ blocks <- readDoc "an emphasis::italic:: word\n"
      blocks `shouldBe`
        [Para [Str "an", Space, Emph [Str "italic"], Space, Str "word"]]

    it "strong:: works the same" $ do
      Pandoc _ blocks <- readDoc "a strong::bold:: word\n"
      blocks `shouldBe`
        [Para [Str "a", Space, Strong [Str "bold"], Space, Str "word"]]

    it "code:: captures verbatim body" $ do
      Pandoc _ blocks <- readDoc "see code::SinOsc.ar:: there\n"
      blocks `shouldBe`
        [Para [Str "see", Space, Code nullAttr "SinOsc.ar", Space, Str "there"]]

    it "link:: with ##label splits target and label" $ do
      Pandoc _ blocks <- readDoc "see link::Classes/SinOsc##the osc::.\n"
      blocks `shouldBe`
        [Para [Str "see", Space
              , Link nullAttr [Str "the osc"] ("Classes/SinOsc", "")
              , Str "."]]

    -- Reader-side link canonicalization mirrors the writer.

    it "link:: strips .html suffix from internal target" $ do
      Pandoc _ blocks <- readDoc "see link::Foo.html#anchor#display::.\n"
      blocks `shouldBe`
        [Para [Str "see", Space
              , Link nullAttr [Str "display"] ("Foo#anchor", "")
              , Str "."]]

    it "link:: drops label when it equals the URL's basename" $ do
      Pandoc _ blocks <- readDoc "see link::Reference/randomSeed##randomSeed::.\n"
      blocks `shouldBe`
        [Para [Str "see", Space
              , Link nullAttr [] ("Reference/randomSeed", "")
              , Str "."]]

    it "link:: trims trailing # from the body before splitting" $ do
      Pandoc _ blocks <- readDoc "see link::Classes/Object#-value#value#::.\n"
      blocks `shouldBe`
        [Para [Str "see", Space
              , Link nullAttr [Str "value"] ("Classes/Object#-value", "")
              , Str "."]]

    -- The trailing-'#' recovery must not touch explicit ## labels.
    it "link:: keeps trailing # in an explicit ## label" $ do
      Pandoc _ blocks <- readDoc "see link::Foo##C#::.\n"
      blocks `shouldBe`
        [Para [Str "see", Space
              , Link nullAttr [Str "C#"] ("Foo", "")
              , Str "."]]


  -- -------------------------------------------------------------------------
  describe "Structural headers" $ do

    it "description:: -> Header 1 'Description'" $ do
      Pandoc _ blocks <- readDoc "description::\n\nintro\n"
      blocks `shouldBe`
        [Header 1 nullAttr [Str "Description"], Para [Str "intro"]]

    it "classmethods:: -> Header 1 'Class Methods'" $ do
      Pandoc _ blocks <- readDoc "classmethods::\n"
      blocks `shouldBe` [Header 1 nullAttr [Str "Class Methods"]]

    it "instancemethods:: -> Header 1 'Instance Methods'" $ do
      Pandoc _ blocks <- readDoc "instancemethods::\n"
      blocks `shouldBe` [Header 1 nullAttr [Str "Instance Methods"]]

    it "examples:: -> Header 1 'Examples'" $ do
      Pandoc _ blocks <- readDoc "examples::\n"
      blocks `shouldBe` [Header 1 nullAttr [Str "Examples"]]

    -- Titled openers keep their tag name as a class.

    it "section:: Title -> Header 1 with section class" $ do
      Pandoc _ blocks <- readDoc "section:: My Section\n"
      blocks `shouldBe`
        [Header 1 ("", ["section"], []) [Str "My", Space, Str "Section"]]

    it "subsection:: Title -> Header 2 with subsection class" $ do
      Pandoc _ blocks <- readDoc "subsection:: Details\n"
      blocks `shouldBe`
        [Header 2 ("", ["subsection"], []) [Str "Details"]]

    it "subsubsection:: Title -> Header 3 with subsubsection class" $ do
      Pandoc _ blocks <- readDoc "subsubsection:: Notes\n"
      blocks `shouldBe`
        [Header 3 ("", ["subsubsection"], []) [Str "Notes"]]

    it "section title accepts inline markup" $ do
      Pandoc _ blocks <- readDoc "section:: A emphasis::cool:: title\n"
      blocks `shouldBe`
        [Header 1 ("", ["section"], [])
          [Str "A", Space, Emph [Str "cool"], Space, Str "title"]]

    -- Canonical section titles collapse to named-section AST shape.

    it "section:: Description -> Header 1 nullAttr (matches description::)" $ do
      Pandoc _ blocks <- readDoc "section:: Description\n"
      blocks `shouldBe` [Header 1 nullAttr [Str "Description"]]

    it "section:: Class Methods -> Header 1 nullAttr (canonical form)" $ do
      Pandoc _ blocks <- readDoc "section:: Class Methods\n"
      blocks `shouldBe` [Header 1 nullAttr [Str "Class Methods"]]

    it "section:: Examples -> Header 1 nullAttr" $ do
      Pandoc _ blocks <- readDoc "section:: Examples\n"
      blocks `shouldBe` [Header 1 nullAttr [Str "Examples"]]

    it "section:: with a non-canonical title keeps the section class" $ do
      Pandoc _ blocks <- readDoc "section:: Class Variables\n"
      blocks `shouldBe`
        [Header 1 ("", ["section"], [])
          [Str "Class", Space, Str "Variables"]]

    it "section:: with a canonical-name *plus markup* does NOT collapse" $ do
      -- Markup-bearing titles must not be canonicalized away.
      Pandoc _ blocks <- readDoc "section:: Description emphasis::extra::\n"
      blocks `shouldBe`
        [Header 1 ("", ["section"], [])
          [Str "Description", Space, Emph [Str "extra"]]]

    it "section:: with inline code in a canonical name does NOT collapse" $ do
      Pandoc _ blocks <- readDoc "section:: code::Description::\n"
      blocks `shouldBe`
        [Header 1 ("", ["section"], [])
          [Code nullAttr "Description"]]

    it "soft:: -> Strikeout" $ do
      Pandoc _ blocks <- readDoc "a soft::muted:: word\n"
      blocks `shouldBe`
        [Para [Str "a", Space, Strikeout [Str "muted"], Space, Str "word"]]

    it "teletype:: -> Span with teletype class" $ do
      Pandoc _ blocks <- readDoc "see teletype::sclang:: there\n"
      blocks `shouldBe`
        [Para [Str "see", Space
              , Span ("", ["teletype"], []) [Str "sclang"]
              , Space, Str "there"]]

    it "inline math:: -> Math InlineMath" $ do
      Pandoc _ blocks <- readDoc "value math::x^2:: here\n"
      blocks `shouldBe`
        [Para [Str "value", Space
              , Math InlineMath "x^2"
              , Space, Str "here"]]

    it "image::url::" $ do
      Pandoc _ blocks <- readDoc "image::images/foo.png::\n"
      blocks `shouldBe`
        [Para [Image nullAttr [] ("images/foo.png", "")]]

    it "image::url#caption:: maps caption into alt" $ do
      Pandoc _ blocks <- readDoc "image::images/foo.png#A caption::\n"
      blocks `shouldBe`
        [Para [Image nullAttr [Str "A caption"] ("images/foo.png", "")]]

    it "image::url##link:: stores link as kv attr" $ do
      Pandoc _ blocks <- readDoc "image::images/foo.png##Classes/Bar::\n"
      blocks `shouldBe`
        [Para [Image ("", [], [("link", "Classes/Bar")])
                     [] ("images/foo.png", "")]]

    it "anchor:: -> raw SCDoc inline" $ do
      Pandoc _ blocks <- readDoc "anchor::my-anchor::\n"
      blocks `shouldBe`
        [Para [RawInline (Format "schelp") "anchor::my-anchor::"]]


  -- -------------------------------------------------------------------------
  describe "Range blocks" $ do

    it "note:: ... :: -> BlockQuote" $ do
      Pandoc _ blocks <- readDoc "note::\nbe careful\n::\n"
      blocks `shouldBe`
        [BlockQuote [Para [Str "be", Space, Str "careful"]]]

    it "warning:: ... :: -> Div with warning class" $ do
      Pandoc _ blocks <- readDoc "warning::\ndanger\n::\n"
      blocks `shouldBe`
        [Div ("", ["warning"], []) [Para [Str "danger"]]]

    it "note:: body may contain multiple paragraphs" $ do
      Pandoc _ blocks <- readDoc "note::\nfirst\n\nsecond\n::\n"
      blocks `shouldBe`
        [BlockQuote [Para [Str "first"], Para [Str "second"]]]

    it "code:: block form -> CodeBlock" $ do
      Pandoc _ blocks <- readDoc "code::\nSinOsc.ar(440)\n::\n"
      blocks `shouldBe`
        [CodeBlock nullAttr "SinOsc.ar(440)"]

    it "teletype:: block form -> CodeBlock with teletype class" $ do
      Pandoc _ blocks <- readDoc "teletype::\nsclang foo\n::\n"
      blocks `shouldBe`
        [CodeBlock ("", ["teletype"], []) "sclang foo"]

    it "math:: block form -> CodeBlock with math class" $ do
      Pandoc _ blocks <- readDoc "math::\nE = mc^2\n::\n"
      blocks `shouldBe`
        [CodeBlock ("", ["math"], []) "E = mc^2"]

    it "stray :: inside a paragraph ends it cleanly (defensive fallback)" $ do
      -- A standalone TagSym should terminate the current paragraph.
      Pandoc _ blocks <- readDoc "note::\nhello\n::\n"
      blocks `shouldBe`
        [BlockQuote [Para [Str "hello"]]]


  -- -------------------------------------------------------------------------
  describe "Method sections" $ do

    it "method:: ar -> Header 2 with method class" $ do
      Pandoc _ blocks <- readDoc "method:: ar\n"
      blocks `shouldBe` [Header 2 ("", ["method"], []) [Str "ar"]]

    it "method:: ar, kr -> Header 2 with comma-joined names" $ do
      Pandoc _ blocks <- readDoc "method:: ar, kr\n"
      blocks `shouldBe` [Header 2 ("", ["method"], []) [Str "ar, kr"]]

    it "method:: ar(freq, amp) -> args attr keeps parens (upstream-faithful)" $ do
      -- METHODARGS includes the parentheses.
      Pandoc _ blocks <- readDoc "method:: ar(freq, amp)\n"
      blocks `shouldBe`
        [Header 2 ("", ["method"], [("args", "(freq, amp)")]) [Str "ar"]]

    it "argument:: freq -> Header 3 with argument class" $ do
      Pandoc _ blocks <- readDoc "argument:: freq\n"
      blocks `shouldBe` [Header 3 ("", ["argument"], []) [Str "freq"]]

    it "returns:: -> Header 3 with returns class and empty inlines" $ do
      Pandoc _ blocks <- readDoc "returns::\n"
      blocks `shouldBe` [Header 3 ("", ["returns"], []) []]

    it "discussion:: -> Header 3 with discussion class" $ do
      Pandoc _ blocks <- readDoc "discussion::\n"
      blocks `shouldBe` [Header 3 ("", ["discussion"], []) []]

    -- Only @classmethods::@ changes @private::@ to cprivate.
    it "private:: helper at file level -> Header 2 with iprivate class" $ do
      Pandoc _ blocks <- readDoc "private:: helper\n"
      blocks `shouldBe` [Header 2 ("", ["private", "iprivate"], []) [Str "helper"]]

    it "copymethod:: SinOsc ar -> Header 2 with copymethod class" $ do
      Pandoc _ blocks <- readDoc "copymethod:: SinOsc ar\n"
      blocks `shouldBe`
        [Header 2 ("", ["copymethod"], [])
                  [Str "SinOsc", Space, Str "ar"]]

    it "classtree:: SinOsc -> Header 2 with classtree class" $ do
      Pandoc _ blocks <- readDoc "classtree:: SinOsc\n"
      blocks `shouldBe`
        [Header 2 ("", ["classtree"], []) [Str "SinOsc"]]

    -- Body-position keyword:: is metadata, not visible content.
    it "body-level keyword:: foo, bar lifts into Meta keywords" $ do
      let input =
            "description::\n\
            \\n\
            \body\n\
            \\n\
            \keyword:: foo, bar\n"
      Pandoc meta blocks <- readDoc input
      Map.lookup "keywords" (unMeta meta)
        `shouldBe` Just (MetaList [MetaString "foo", MetaString "bar"])
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Description"]
        , Para [Str "body"]
        ]

    -- Keyword metadata is deduped in source order.
    it "body keyword:: a, a dedupes to a single MetaString" $ do
      Pandoc meta _ <- readDoc "description::\n\nbody\n\nkeyword:: a, a\n"
      Map.lookup "keywords" (unMeta meta)
        `shouldBe` Just (MetaList [MetaString "a"])

    it "header keyword:: a, a dedupes to a single MetaString" $ do
      Pandoc meta _ <- readDoc "keyword:: a, a\n"
      Map.lookup "keywords" (unMeta meta)
        `shouldBe` Just (MetaList [MetaString "a"])

    it "keyword in both header and body merges and dedupes" $ do
      let input =
            "keyword:: a\n\
            \\n\
            \description::\n\
            \\n\
            \keyword:: a, b\n"
      Pandoc meta _ <- readDoc input
      Map.lookup "keywords" (unMeta meta)
        `shouldBe` Just (MetaList [MetaString "a", MetaString "b"])

    -- Section context adds cmethod/imethod and related classes.

    it "method:: in classmethods:: gets cmethod class" $ do
      Pandoc _ blocks <- readDoc "classmethods::\n\nmethod:: ar\n"
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Class Methods"]
        , Header 2 ("", ["method", "cmethod"], []) [Str "ar"]
        ]

    it "method:: in instancemethods:: gets imethod class" $ do
      Pandoc _ blocks <- readDoc "instancemethods::\n\nmethod:: play\n"
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Instance Methods"]
        , Header 2 ("", ["method", "imethod"], []) [Str "play"]
        ]

    it "method:: in description:: gets no context class" $ do
      Pandoc _ blocks <- readDoc "description::\n\nmethod:: foo\n"
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Description"]
        , Header 2 ("", ["method"], []) [Str "foo"]
        ]

    it "private:: in classmethods:: gets cprivate class" $ do
      Pandoc _ blocks <- readDoc "classmethods::\n\nprivate:: helper\n"
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Class Methods"]
        , Header 2 ("", ["private", "cprivate"], []) [Str "helper"]
        ]

    it "private:: in instancemethods:: gets iprivate class" $ do
      Pandoc _ blocks <- readDoc "instancemethods::\n\nprivate:: helper\n"
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Instance Methods"]
        , Header 2 ("", ["private", "iprivate"], []) [Str "helper"]
        ]

    -- @private::@ defaults to iprivate outside class methods.
    it "private:: in description:: gets iprivate class" $ do
      Pandoc _ blocks <- readDoc "description::\n\nprivate:: helper\n"
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Description"]
        , Header 2 ("", ["private", "iprivate"], []) [Str "helper"]
        ]

    it "private:: in examples:: gets iprivate class" $ do
      Pandoc _ blocks <- readDoc "examples::\n\nprivate:: helper\n"
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Examples"]
        , Header 2 ("", ["private", "iprivate"], []) [Str "helper"]
        ]

    it "private:: in generic section:: gets iprivate class" $ do
      Pandoc _ blocks <- readDoc "section:: Notes\n\nprivate:: helper\n"
      blocks `shouldBe`
        [ Header 1 ("", ["section"], []) [Str "Notes"]
        , Header 2 ("", ["private", "iprivate"], []) [Str "helper"]
        ]

    it "copymethod:: in classmethods:: gets ccopymethod class" $ do
      Pandoc _ blocks <- readDoc "classmethods::\n\ncopymethod:: SinOsc ar\n"
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Class Methods"]
        , Header 2 ("", ["copymethod", "ccopymethod"], [])
                  [Str "SinOsc", Space, Str "ar"]
        ]

    it "context resets when a new top-level section opens" $ do
      let input =
            "classmethods::\n\
            \\n\
            \method:: ar\n\
            \\n\
            \examples::\n\
            \\n\
            \method:: foo\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Class Methods"]
        , Header 2 ("", ["method", "cmethod"], []) [Str "ar"]
        , Header 1 nullAttr [Str "Examples"]
        -- No cmethod here — InExamples doesn't add a context class.
        , Header 2 ("", ["method"], []) [Str "foo"]
        ]

    it "section:: Class Methods (canonical collapse) also sets cmethod context" $ do
      let input =
            "section:: Class Methods\n\
            \\n\
            \method:: ar\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Class Methods"]
        , Header 2 ("", ["method", "cmethod"], []) [Str "ar"]
        ]

    it "method body with argument:: and returns:: compose correctly" $ do
      let input =
            "classmethods::\n\
            \\n\
            \method:: ar\n\
            \\n\
            \Construct a UGen.\n\
            \\n\
            \argument:: freq\n\
            \\n\
            \Frequency in Hz.\n\
            \\n\
            \returns::\n\
            \\n\
            \A UGen.\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Class Methods"]
        -- MethodContext preserves the class-method distinction.
        , Header 2 ("", ["method", "cmethod"], []) [Str "ar"]
        , Para [Str "Construct", Space, Str "a", Space, Str "UGen", Str "."]
        , Header 3 ("", ["argument"], []) [Str "freq"]
        , Para [Str "Frequency", Space, Str "in", Space, Str "Hz", Str "."]
        , Header 3 ("", ["returns"], []) []
        , Para [Str "A", Space, Str "UGen", Str "."]
        ]

    -- Method-body tags are represented as flat sibling headers.

    it "body-only method:: emits flat Header + Para (no args/returns/discussion)" $ do
      let input =
            "method:: ar\n\
            \\n\
            \Construct a UGen.\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [ Header 2 ("", ["method"], []) [Str "ar"]
        , Para [Str "Construct", Space, Str "a", Space, Str "UGen", Str "."]
        ]

    -- Empty @argument::@ gets an empty heading and sibling body.
    it "argument:: with no title still takes following body" $ do
      let input =
            "argument::\n\
            \\n\
            \Body text.\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [ Header 3 ("", ["argument"], []) []
        , Para [Str "Body", Space, Str "text", Str "."]
        ]

    it "two argument:: in sequence emit sibling Header 3 + Para pairs" $ do
      let input =
            "argument:: freq\n\
            \\n\
            \Frequency.\n\
            \\n\
            \argument:: amp\n\
            \\n\
            \Amplitude.\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [ Header 3 ("", ["argument"], []) [Str "freq"]
        , Para [Str "Frequency", Str "."]
        , Header 3 ("", ["argument"], []) [Str "amp"]
        , Para [Str "Amplitude", Str "."]
        ]

    it "discussion:: takes following body as sibling Para" $ do
      let input =
            "discussion::\n\
            \\n\
            \Some discussion.\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [ Header 3 ("", ["discussion"], []) []
        , Para [Str "Some", Space, Str "discussion", Str "."]
        ]

    it "full canonical methodbody: body / arg / returns / discussion" $ do
      let input =
            "classmethods::\n\
            \\n\
            \method:: ar\n\
            \\n\
            \Construct a UGen.\n\
            \\n\
            \argument:: freq\n\
            \\n\
            \Frequency.\n\
            \\n\
            \returns::\n\
            \\n\
            \A UGen.\n\
            \\n\
            \discussion::\n\
            \\n\
            \Notes here.\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [ Header 1 nullAttr [Str "Class Methods"]
        , Header 2 ("", ["method", "cmethod"], []) [Str "ar"]
        , Para [Str "Construct", Space, Str "a", Space, Str "UGen", Str "."]
        , Header 3 ("", ["argument"], []) [Str "freq"]
        , Para [Str "Frequency", Str "."]
        , Header 3 ("", ["returns"], []) []
        , Para [Str "A", Space, Str "UGen", Str "."]
        , Header 3 ("", ["discussion"], []) []
        , Para [Str "Notes", Space, Str "here", Str "."]
        ]

    -- Flat method-body representation accepts this order.
    it "out-of-order: returns:: before argument:: parses as flat siblings" $ do
      let input =
            "method:: ar\n\
            \\n\
            \returns::\n\
            \\n\
            \A thing.\n\
            \\n\
            \argument:: freq\n\
            \\n\
            \Frequency.\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [ Header 2 ("", ["method"], []) [Str "ar"]
        , Header 3 ("", ["returns"], []) []
        , Para [Str "A", Space, Str "thing", Str "."]
        , Header 3 ("", ["argument"], []) [Str "freq"]
        , Para [Str "Frequency", Str "."]
        ]

    -- Flat method-body representation accepts repeated returns::.
    it "multiple returns:: under one method:: parse as sibling Header 3 blocks" $ do
      let input =
            "method:: ar\n\
            \\n\
            \returns::\n\
            \\n\
            \First.\n\
            \\n\
            \returns::\n\
            \\n\
            \Second.\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [ Header 2 ("", ["method"], []) [Str "ar"]
        , Header 3 ("", ["returns"], []) []
        , Para [Str "First", Str "."]
        , Header 3 ("", ["returns"], []) []
        , Para [Str "Second", Str "."]
        ]


  -- -------------------------------------------------------------------------
  describe "Lists and trees" $ do

    it "list:: -> BulletList with one item per ##" $ do
      Pandoc _ blocks <- readDoc "list::\n## first\n## second\n::\n"
      blocks `shouldBe`
        [BulletList
          [ [Para [Str "first"]]
          , [Para [Str "second"]]
          ]]

    it "numberedlist:: -> OrderedList" $ do
      Pandoc _ blocks <- readDoc "numberedlist::\n## one\n## two\n::\n"
      blocks `shouldBe`
        [OrderedList (1, DefaultStyle, DefaultDelim)
          [ [Para [Str "one"]]
          , [Para [Str "two"]]
          ]]

    it "tree:: -> Div with tree class wrapping a BulletList" $ do
      Pandoc _ blocks <- readDoc "tree::\n## SinOsc\n## UGen\n::\n"
      blocks `shouldBe`
        [Div ("", ["tree"], [])
          [BulletList
            [ [Para [Str "SinOsc"]]
            , [Para [Str "UGen"]]
            ]]]

    it "list:: item bodies can span multiple blocks" $ do
      let input =
            "list::\n\
            \## first item\n\
            \\n\
            \second paragraph of first item\n\
            \\n\
            \## second item\n\
            \::\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [BulletList
          [ [ Para [Str "first", Space, Str "item"]
            , Para [Str "second", Space, Str "paragraph", Space, Str "of",
                    Space, Str "first", Space, Str "item"]
            ]
          , [Para [Str "second", Space, Str "item"]]
          ]]


  -- -------------------------------------------------------------------------
  describe "Definition lists" $ do

    it "definitionlist:: ## term || def :: -> DefinitionList" $ do
      Pandoc _ blocks <-
        readDoc "definitionlist::\n## Term A\n|| Definition A.\n::\n"
      blocks `shouldBe`
        [DefinitionList
          [ ( [Str "Term", Space, Str "A"]
            -- Each definition is a block list.
            , [[Para [Str "Definition", Space, Str "A", Str "."]]]
            )]]

    it "term with multiple || definitions accumulates them in one row" $ do
      let input =
            "definitionlist::\n\
            \## Term\n\
            \|| def1\n\
            \|| def2\n\
            \::\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [DefinitionList
          [ ( [Str "Term"]
            -- letter run + digit run, per flex parity
            , [ [Para [Str "def", Str "1"]]
              , [Para [Str "def", Str "2"]]
              ]
            )]]

    it "keyword:: on the next line after a term flows through literalTagOpen" $ do
      -- With no blank line, keyword:: is source text inside the term.
      let input =
            "definitionlist::\n\
            \## buffer\n\
            \keyword:: buffer\n\
            \|| A server-side buffer.\n\
            \::\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [DefinitionList
          [ ( [ Str "buffer", Space
              , RawInline (Format "schelp") "keyword::"
              , Str "buffer"
              ]
            , [[ Para [Str "A", Space, Str "server", Str "-", Str "side",
                        Space, Str "buffer", Str "."]
              ]]
            )]]

    it "keyword:: separated from term by a blank line goes through keywordAnnotation" $ do
      -- With a blank line, keyword:: is a term annotation.
      let input =
            "definitionlist::\n\
            \## buffer\n\
            \\n\
            \keyword:: buffer\n\
            \|| A server-side buffer.\n\
            \::\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [DefinitionList
          [ ( [ Str "buffer"
              , LineBreak
              , RawInline (Format "schelp") "keyword:: buffer"
              ]
            , [[ Para [Str "A", Space, Str "server", Str "-", Str "side",
                        Space, Str "buffer", Str "."]
              ]]
            )]]

    it "definition body can span multiple blocks (per upstream grammar)" $ do
      let input =
            "definitionlist::\n\
            \## Term\n\
            \|| first paragraph\n\
            \\n\
            \second paragraph\n\
            \::\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [DefinitionList
          [ ( [Str "Term"]
            , [[ Para [Str "first", Space, Str "paragraph"]
               , Para [Str "second", Space, Str "paragraph"]
               ]]
            )]]

    -- Empty @||@ bodies parse as no definitions.
    it "term with empty || body parses as (term, [])" $ do
      let input =
            "definitionlist::\n\
            \## bare\n\
            \||\n\
            \## body-bearing\n\
            \|| something\n\
            \::\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [DefinitionList
          [ ([Str "bare"],         [])
          , ([Str "body", Str "-", Str "bearing"],
             [[Para [Str "something"]]])
          ]]

    -- Multiple @##@ terms before one @||@ share one body.
    it "shared-body terms parse as one row with scdoc-defterm Spans" $ do
      let input =
            "definitionlist::\n\
            \## one\n\
            \## two\n\
            \## three\n\
            \|| shared body\n\
            \::\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [DefinitionList
          [ ( [ Span ("", ["scdoc-defterm"], []) [Str "one"]
              , LineBreak
              , Span ("", ["scdoc-defterm"], []) [Str "two"]
              , LineBreak
              , Span ("", ["scdoc-defterm"], []) [Str "three"]
              ]
            , [[Para [Str "shared", Space, Str "body"]]]
            )
          ]]


  -- -------------------------------------------------------------------------
  describe "Tables" $ let
      -- Single-line cells become Para; writer renders Plain the same.
      cell xs = Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Para xs]
    in do

    it "table:: ## header || header :: with body rows" $ do
      let input =
            "table::\n\
            \## Name || Value\n\
            \## freq || 440\n\
            \## amp  || 0.5\n\
            \::\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [Table nullAttr (Caption Nothing []) []
          (TableHead nullAttr
            [Row nullAttr
              [ cell [Str "Name"]
              , cell [Str "Value"]
              ]])
          [TableBody nullAttr (RowHeadColumns 0) []
            [ Row nullAttr [cell [Str "freq"], cell [Str "440"]]
            , Row nullAttr [cell [Str "amp"],  cell [Str "0.5"]]
            ]]
          (TableFoot nullAttr [])]

    it "single-row table puts that row in the header" $ do
      Pandoc _ blocks <- readDoc "table::\n## A || B\n::\n"
      blocks `shouldBe`
        [Table nullAttr (Caption Nothing []) []
          (TableHead nullAttr
            [Row nullAttr [cell [Str "A"], cell [Str "B"]]])
          [TableBody nullAttr (RowHeadColumns 0) [] []]
          (TableFoot nullAttr [])]


  -- -------------------------------------------------------------------------
  describe "Footnotes" $ do

    it "footnote:: in prose -> inline Note" $ do
      Pandoc _ blocks <- readDoc "see footnote::\nthe details\n::.\n"
      blocks `shouldBe`
        [Para
          [ Str "see", Space
          , Note [Para [Str "the", Space, Str "details"]]
          , Str "."
          ]]


  -- -------------------------------------------------------------------------
  describe "Structural composition" $ do

    it "structural headers compose into a flat block list" $ do
      let input =
            "description::\n\
            \intro paragraph\n\
            \\n\
            \classmethods::\n\
            \\n\
            \section:: Overview\n\
            \\n\
            \overview text\n"
      Pandoc _ blocks <- readDoc input
      blocks `shouldBe`
        [ Header 1 nullAttr                  [Str "Description"]
        , Para [Str "intro", Space, Str "paragraph"]
        , Header 1 nullAttr                  [Str "Class Methods"]
        , Header 1 ("", ["section"], [])     [Str "Overview"]
        , Para [Str "overview", Space, Str "text"]
        ]
