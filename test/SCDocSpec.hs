{-# LANGUAGE OverloadedStrings #-}

module SCDocSpec (spec) where

import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Test.Hspec

import qualified Text.DocTemplates          as DT
import qualified Text.Pandoc                as Pandoc
import           Text.Pandoc.Definition
import           Text.Pandoc.Options        (ReaderOptions (..),
                                             WriterOptions (..), def,
                                             pandocExtensions)
import           Text.Pandoc.Writers.SCDoc
import           Text.Pandoc.Writers.Shared (defField)


doc :: [Block] -> Pandoc
doc = Pandoc nullMeta

docWithMeta :: [(Text, MetaValue)] -> [Block] -> Pandoc
docWithMeta kvs = Pandoc (Meta (Map.fromList kvs))

-- render a single block stripping surrounding whitespace
renderOne :: Block -> Text
renderOne b = T.strip $ writeSCDocPure (doc [b])

-- render a single Para of inlines
renderPara :: [Inline] -> Text
renderPara xs = T.strip $ writeSCDocPure (doc [Para xs])

mdReaderOpts :: ReaderOptions
mdReaderOpts = def { readerExtensions = pandocExtensions }

-- text assertion, hspec's shouldContain only works on lists
shouldContainT :: HasCallStack => Text -> Text -> Expectation
shouldContainT haystack needle =
  haystack `shouldSatisfy` T.isInfixOf needle

shouldNotContainT :: HasCallStack => Text -> Text -> Expectation
shouldNotContainT haystack needle =
  haystack `shouldSatisfy` (not . T.isInfixOf needle)

infix 1 `shouldContainT`, `shouldNotContainT`


spec :: Spec
spec = do

  -- -------------------------------------------------------------------------
  describe "hasClass" $ do

    it "finds a matching class" $
      hasClass "foo" ["foo", "bar"] `shouldBe` True

    it "is case-insensitive" $
      hasClass "Method" ["method"] `shouldBe` True

    it "returns False when absent" $
      hasClass "baz" ["foo", "bar"] `shouldBe` False

    it "returns False on empty list" $
      hasClass "foo" [] `shouldBe` False


  -- -------------------------------------------------------------------------
  describe "isExternalUrl" $ do

    it "recognises https://" $
      isExternalUrl "https://example.com" `shouldBe` True

    it "recognises http://" $
      isExternalUrl "http://example.com" `shouldBe` True

    it "recognises ftp://" $
      isExternalUrl "ftp://example.com" `shouldBe` True

    it "recognises mailto:" $
      isExternalUrl "mailto:foo@bar.com" `shouldBe` True

    it "recognises file://" $
      isExternalUrl "file:///usr/share/doc" `shouldBe` True

    it "returns False for a bare class name" $
      isExternalUrl "SomeClass" `shouldBe` False

    it "returns False for an anchor-only target" $
      isExternalUrl "#section" `shouldBe` False

    it "returns False for a relative path" $
      isExternalUrl "Classes/MySynth" `shouldBe` False


  -- -------------------------------------------------------------------------
  describe "linkHasAnchor" $ do

    it "True for target with single #" $
      linkHasAnchor "Foo#bar" `shouldBe` True

    it "False when no # present" $
      linkHasAnchor "Foo" `shouldBe` False

    it "False when ## present (label separator, not anchor)" $
      linkHasAnchor "Foo##label" `shouldBe` False

    it "False for external URL (even with #)" $
      linkHasAnchor "https://example.com#section" `shouldBe` False

    it "False for empty string" $
      linkHasAnchor "" `shouldBe` False


  -- -------------------------------------------------------------------------
  describe "isRawSCDocFormat" $ do

    it "True for schelp" $
      isRawSCDocFormat (Format "schelp") `shouldBe` True

    it "True for scdoc" $
      isRawSCDocFormat (Format "scdoc") `shouldBe` True

    it "is case-insensitive" $
      isRawSCDocFormat (Format "SCHELP") `shouldBe` True

    it "False for html" $
      isRawSCDocFormat (Format "html") `shouldBe` False

    it "False for org" $
      isRawSCDocFormat (Format "org") `shouldBe` False


  -- -------------------------------------------------------------------------
  describe "scHeader" $ do

    it "emits title::" $
      writeSCDocPure (docWithMeta [("title", MetaString "MySynth")] [])
        `shouldContainT` "title:: MySynth"

    it "emits summary::" $
      writeSCDocPure (docWithMeta [("summary", MetaString "A resonant filter")] [])
        `shouldContainT` "summary:: A resonant filter"

    it "emits categories:: from a MetaList" $
      writeSCDocPure (docWithMeta [("categories", MetaList [MetaString "UGens", MetaString "Filters"])] [])
        `shouldContainT` "categories:: UGens, Filters"

    it "emits related::" $
      writeSCDocPure (docWithMeta [("related", MetaString "Classes/SinOsc")] [])
        `shouldContainT` "related:: Classes/SinOsc"

    it "emits redirect::" $
      writeSCDocPure (docWithMeta [("redirect", MetaString "Classes/NewName")] [])
        `shouldContainT` "redirect:: Classes/NewName"

    it "omits a field whose value is empty" $
      writeSCDocPure (docWithMeta [("summary", MetaString "")] [])
        `shouldNotContainT` "summary::"

    it "ignores meta keys that are not SCDoc header fields" $
      writeSCDocPure (docWithMeta [("author", MetaString "Alice")] [])
        `shouldNotContainT` "author::"

    it "flattens a multi-word MetaInlines value onto one line" $
      writeSCDocPure (docWithMeta [("title", MetaInlines [Str "My", Space, Str "Synth"])] [])
        `shouldContainT` "title:: My Synth"

    it "MetaBool is ignored (no SCDoc equivalent)" $
      writeSCDocPure (docWithMeta [("title", MetaBool True)] [])
        `shouldNotContainT` "title::"


  -- -------------------------------------------------------------------------
  describe "keyword injection" $ do

    it "emits keyword:: tag" $
      writeSCDocPure (docWithMeta [("keywords", MetaString "filter")] [])
        `shouldContainT` "keyword:: filter"

    it "handles a MetaList of keywords" $ do
      let out = writeSCDocPure (docWithMeta [("keywords", MetaList [MetaString "filter", MetaString "ugen"])] [])
      out `shouldContainT` "keyword:: filter"
      out `shouldContainT` "keyword:: ugen"

    it "deduplicates keywords" $ do
      let out = writeSCDocPure (docWithMeta [("keywords", MetaList [MetaString "filter", MetaString "filter"])] [])
      T.count "filter" out `shouldBe` 1

    it "merges keyword and keywords fields" $ do
      let out = writeSCDocPure (docWithMeta [("keyword", MetaString "filter"), ("keywords", MetaString "ugen")] [])
      out `shouldContainT` "filter"
      out `shouldContainT` "ugen"

    it "inserts keyword:: after description:: when a description heading exists" $ do
      let d   = docWithMeta [("keywords", MetaString "filter")]
                  [Header 1 nullAttr [Str "Description"], Para [Str "body"]]
          out = writeSCDocPure d
          -- keyword:: must appear after description:: in the output
          (prePart, _) = T.breakOn "keyword::" out
      prePart `shouldContainT` "description::"

    it "appends keyword:: after body content when no description heading exists" $ do
      let d   = docWithMeta [("keywords", MetaString "filter")]
                  [Para [Str "intro"]]
          out = writeSCDocPure d
          (prePart, _) = T.breakOn "keyword::" out
      prePart `shouldContainT` "intro"

    it "omits keyword:: entirely when no keywords are present" $
      writeSCDocPure (docWithMeta [("title", MetaString "Foo")] [])
        `shouldNotContainT` "keyword::"


  -- -------------------------------------------------------------------------
  describe "renderBlock" $ do

    describe "Para / Plain" $ do
      it "renders paragraph text" $
        renderOne (Para [Str "hello world"]) `shouldBe` "hello world"

      it "renders Plain the same as Para" $
        renderOne (Plain [Str "hello"]) `shouldBe` "hello"

    describe "LineBlock" $ do
      it "joins lines with newlines" $ do
        let out = renderOne (LineBlock [[Str "line one"], [Str "line two"]])
        out `shouldContainT` "line one"
        out `shouldContainT` "line two"
        T.count "\n" out `shouldBe` 1

    describe "CodeBlock" $ do
      it "wraps code in code:: block by default" $ do
        let out = renderOne (CodeBlock nullAttr "x = 1")
        out `shouldContainT` "code::"
        out `shouldContainT` "x = 1"

      it "passes schelp code through verbatim" $
        renderOne (CodeBlock ("", ["schelp"], []) "method:: foo")
          `shouldBe` "method:: foo"

      it "passes scdoc code through verbatim" $
        renderOne (CodeBlock ("", ["scdoc"], []) "section:: A")
          `shouldBe` "section:: A"

      it "wraps teletype code in teletype:: block" $
        renderOne (CodeBlock ("", ["teletype"], []) "foo")
          `shouldContainT` "teletype::"

      it "wraps math code in math:: block" $
        renderOne (CodeBlock ("", ["math"], []) "x^2")
          `shouldContainT` "math::"

    describe "BlockQuote" $ do
      -- SCDoc has no generic blockquote; the closest mapping is note::
      it "maps to note:: range block" $ do
        let out = renderOne (BlockQuote [Para [Str "important"]])
        out `shouldContainT` "note::"
        out `shouldContainT` "important"

    describe "BulletList" $ do
      it "renders as list:: with ## items" $ do
        let out = renderOne (BulletList [[Para [Str "a"]], [Para [Str "b"]]])
        out `shouldContainT` "list::"
        out `shouldContainT` "## a"
        out `shouldContainT` "## b"

      it "flattens nested BulletList to sibling ## items" $ do
        let out = renderOne (BulletList
                    [ [Para [Str "outer"], BulletList [[Para [Str "inner a"]], [Para [Str "inner b"]]]]
                    , [Para [Str "last"]]
                    ])
        out `shouldContainT` "## outer"
        out `shouldContainT` "## inner a"
        out `shouldContainT` "## inner b"
        out `shouldContainT` "## last"
        out `shouldNotContainT` "list::\n## outer\nlist::"  -- no nested list:: blocks

      it "flattens nested OrderedList inside BulletList" $ do
        let out = renderOne (BulletList
                    [ [Para [Str "top"], OrderedList (1, DefaultStyle, DefaultDelim)
                        [[Para [Str "sub 1"]], [Para [Str "sub 2"]]]]
                    ])
        out `shouldContainT` "## top"
        out `shouldContainT` "## sub 1"
        out `shouldContainT` "## sub 2"

    describe "OrderedList" $ do
      it "renders as numberedlist::" $ do
        let out = renderOne (OrderedList (1, DefaultStyle, DefaultDelim)
                              [[Para [Str "first"]], [Para [Str "second"]]])
        out `shouldContainT` "numberedlist::"
        out `shouldContainT` "## first"
        out `shouldContainT` "## second"

      it "flattens nested OrderedList to sibling ## items" $ do
        let out = renderOne (OrderedList (1, DefaultStyle, DefaultDelim)
                    [ [Para [Str "one"], OrderedList (1, DefaultStyle, DefaultDelim)
                        [[Para [Str "one-a"]], [Para [Str "one-b"]]]]
                    , [Para [Str "two"]]
                    ])
        out `shouldContainT` "## one"
        out `shouldContainT` "## one-a"
        out `shouldContainT` "## one-b"
        out `shouldContainT` "## two"

    describe "HorizontalRule" $ do
      -- SCDoc has no horizontal rule; suppress rather than emit garbage
      it "produces no output" $
        renderOne HorizontalRule `shouldBe` ""

    describe "structural Header" $ do
      it "level 1 'Description' -> description::" $
        renderOne (Header 1 nullAttr [Str "Description"])
          `shouldContainT` "description::"

      it "level 1 'Class Methods' -> classmethods::" $
        renderOne (Header 1 nullAttr [Str "Class Methods"])
          `shouldContainT` "classmethods::"

      it "level 1 'ClassMethods' -> classmethods::" $
        renderOne (Header 1 nullAttr [Str "ClassMethods"])
          `shouldContainT` "classmethods::"

      it "level 1 'Instance Methods' -> instancemethods::" $
        renderOne (Header 1 nullAttr [Str "Instance Methods"])
          `shouldContainT` "instancemethods::"

      it "level 1 'Examples' -> examples::" $
        renderOne (Header 1 nullAttr [Str "Examples"])
          `shouldContainT` "examples::"

      it "level 1 arbitrary title -> section:: title" $
        renderOne (Header 1 nullAttr [Str "Overview"])
          `shouldContainT` "section:: Overview"

      it "level 2 -> subsection::" $
        renderOne (Header 2 nullAttr [Str "Details"])
          `shouldContainT` "subsection:: Details"

      it "level 3 -> subsubsection::" $
        renderOne (Header 3 nullAttr [Str "Notes"])
          `shouldContainT` "subsubsection:: Notes"

      it "level 4 and above -> subsubsection::" $
        renderOne (Header 4 nullAttr [Str "Deep"])
          `shouldContainT` "subsubsection:: Deep"

      it "anchor= attribute emits anchor:: tag after the section header" $ do
        let out = renderOne (Header 2 ("", [], [("anchor", "my-anchor")]) [Str "Details"])
        out `shouldContainT` "subsection:: Details"
        out `shouldContainT` "anchor::my-anchor::"

      it "no anchor= attribute emits no anchor:: tag" $
        renderOne (Header 2 ("", [], []) [Str "Details"])
          `shouldNotContainT` "anchor::"

    describe "SCDoc-class Header" $ do
      it "method class -> method:: name" $
        renderOne (Header 2 ("", ["method"], []) [Str "play"])
          `shouldContainT` "method:: play"

      it "method class strips trailing _ from setter name" $ do
        let out = renderOne (Header 2 ("", ["method"], []) [Str "freq_"])
        out `shouldContainT` "method:: freq"
        out `shouldNotContainT` "freq_"

      it "single-character _ name is left unchanged (length guard)" $
        renderOne (Header 2 ("", ["method"], []) [Str "_"])
          `shouldContainT` "method:: _"

      it "multiple comma-separated names are preserved" $
        renderOne (Header 2 ("", ["method"], []) [Str "ar, kr"])
          `shouldContainT` "method:: ar, kr"

      it "method class with args= attr appends argument list" $
        renderOne (Header 2 ("", ["method"], [("args", "freq=440, amp=0.1")]) [Str "ar"])
          `shouldBe` "method:: ar freq=440, amp=0.1"

      it "argument class -> argument:: name" $
        renderOne (Header 3 ("", ["argument"], []) [Str "freq"])
          `shouldContainT` "argument:: freq"

      it "returns class -> returns::" $
        renderOne (Header 3 ("", ["returns"], []) [Str "anything"])
          `shouldContainT` "returns::"

      it "returns class with text= attr includes inline description" $
        renderOne (Header 3 ("", ["returns"], [("text", "a UGen")]) [])
          `shouldBe` "returns:: a UGen"

      it "discussion class -> discussion::" $
        renderOne (Header 3 ("", ["discussion"], []) [Str "anything"])
          `shouldContainT` "discussion::"

      it "heading text Discussion -> discussion:: (any level)" $
        renderOne (Header 2 nullAttr [Str "Discussion"])
          `shouldBe` "discussion::"

      it "heading text Returns -> returns::" $
        renderOne (Header 2 nullAttr [Str "Returns"])
          `shouldBe` "returns::"

      it "heading text Returns with text= attr -> returns:: with description" $
        renderOne (Header 2 ("", [], [("text", "a UGen")]) [Str "Returns"])
          `shouldBe` "returns:: a UGen"

      it "copymethod class with class+method attrs -> copymethod::" $
        renderOne (Header 2 ("", ["copymethod"], [("class", "SinOsc"), ("method", "ar")]) [])
          `shouldBe` "copymethod:: SinOsc ar"

      it "copymethod class with source+method attrs -> copymethod::" $
        renderOne (Header 2 ("", ["copymethod"], [("source", "SinOsc"), ("method", "ar")]) [])
          `shouldBe` "copymethod:: SinOsc ar"

      it "copymethod class with Class method heading -> copymethod::" $
        renderOne (Header 2 ("", ["copymethod"], []) [Str "SinOsc", Space, Str "ar"])
          `shouldBe` "copymethod:: SinOsc ar"

      it "copymethod class with Class.method heading -> copymethod::" $
        renderOne (Header 2 ("", ["copymethod"], []) [Str "SinOsc.ar"])
          `shouldBe` "copymethod:: SinOsc ar"

      it "copymethod class with missing attrs falls back to method:: with heading text" $ do
        let out = renderOne (Header 2 ("", ["copymethod"], [])
                              [Str "SinOsc", Space, Str "ar", Space, Str "extra"])
        out `shouldContainT` "method:: SinOsc ar extra"
        out `shouldNotContainT` "copymethod::"

      it "private class -> private:: name" $
        renderOne (Header 2 ("", ["private"], []) [Str "internalHelper"])
          `shouldContainT` "private:: internalHelper"

      it "classtree class -> classtree:: name" $
        renderOne (Header 2 ("", ["classtree"], []) [Str "SinOsc"])
          `shouldContainT` "classtree:: SinOsc"

      it "anchor class -> anchor:: modal tag" $
        renderOne (Header 2 ("", ["anchor"], []) [Str "my-anchor"])
          `shouldContainT` "anchor::my-anchor::"

    describe "Div" $ do
      it "note class -> note:: range block" $ do
        let out = renderOne (Div ("", ["note"], []) [Para [Str "careful"]])
        out `shouldContainT` "note::"
        out `shouldContainT` "careful"

      it "warning class -> warning:: range block" $
        renderOne (Div ("", ["warning"], []) [Para [Str "danger"]])
          `shouldContainT` "warning::"

      it "strips Pandoc-generated title Div from note body" $ do
        let titleDiv = Div ("", ["title"], []) [Para [Str "Note"]]
            out = renderOne (Div ("", ["note"], []) [titleDiv, Para [Str "careful"]])
        out `shouldContainT` "note::"
        out `shouldContainT` "careful"
        out `shouldNotContainT` "note::\nNote"

      it "strips Pandoc-generated title Div from warning body" $ do
        let titleDiv = Div ("", ["title"], []) [Para [Str "Warning"]]
            out = renderOne (Div ("", ["warning"], []) [titleDiv, Para [Str "danger"]])
        out `shouldContainT` "warning::"
        out `shouldContainT` "danger"
        out `shouldNotContainT` "warning::\nWarning"

      it "unknown class is transparent" $ do
        let out = renderOne (Div ("", ["custom"], []) [Para [Str "content"]])
        out `shouldContainT` "content"
        out `shouldNotContainT` "custom::"

      it "tree class with BulletList -> tree:: block" $ do
        let out = renderOne (Div ("", ["tree"], [])
                    [BulletList [[Para [Str "SinOsc"]], [Para [Str "UGen"]]]])
        out `shouldContainT` "tree::"
        out `shouldContainT` "## SinOsc"
        out `shouldContainT` "## UGen"

      it "tree class with OrderedList -> tree:: block" $ do
        let out = renderOne (Div ("", ["tree"], [])
                    [OrderedList (1, DefaultStyle, DefaultDelim)
                      [[Para [Str "first"]], [Para [Str "second"]]]])
        out `shouldContainT` "tree::"
        out `shouldContainT` "## first"

      it "tree class flattens nested lists into sibling ## items" $ do
        let out = renderOne (Div ("", ["tree"], [])
                    [BulletList
                      [ [Para [Str "SinOsc"], BulletList [[Para [Str "UGen"]], [Para [Str "Object"]]]]
                      , [Para [Str "SinOscFB"]]
                      ]])
        out `shouldContainT` "tree::"
        out `shouldContainT` "## SinOsc"
        out `shouldContainT` "## UGen"
        out `shouldContainT` "## Object"
        out `shouldContainT` "## SinOscFB"

      it "tree class with no list falls back to rendering blocks" $ do
        let out = renderOne (Div ("", ["tree"], []) [Para [Str "plain text"]])
        out `shouldContainT` "plain text"
        out `shouldNotContainT` "tree::"

    describe "RawBlock" $ do
      it "schelp raw block passes through verbatim" $
        renderOne (RawBlock (Format "schelp") "anchor::foo::")
          `shouldContainT` "anchor::foo::"

      it "scdoc raw block passes through verbatim" $
        renderOne (RawBlock (Format "scdoc") "section:: A")
          `shouldContainT` "section:: A"

      it "non-SCDoc raw block is dropped" $
        renderOne (RawBlock (Format "html") "<b>bold</b>")
          `shouldBe` ""

    describe "DefinitionList" $ do
      it "renders as definitionlist:: with ## terms and || definitions" $ do
        let out = renderOne (DefinitionList [([Str "term"], [[Para [Str "def"]]])])
        out `shouldContainT` "definitionlist::"
        out `shouldContainT` "## term"
        out `shouldContainT` "|| def"

      it "term with no definition emits ## term and empty ||" $ do
        let out = renderOne (DefinitionList [([Str "bare"], [])])
        out `shouldContainT` "## bare"
        out `shouldContainT` "||"

      it "term with multiple definitions emits one || row per definition" $ do
        let out = renderOne (DefinitionList
                    [([Str "term"], [[Para [Str "def1"]], [Para [Str "def2"]]])])
        out `shouldContainT` "## term"
        out `shouldContainT` "|| def1"
        out `shouldContainT` "|| def2"

    describe "Table" $ do
      let mkCell t  = Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str t]]
          mkRow  cs = Row nullAttr (map mkCell cs)
          mkTable hdr body =
            Table nullAttr (Caption Nothing []) []
              (TableHead nullAttr [mkRow hdr])
              [TableBody nullAttr (RowHeadColumns 0) [] (map mkRow body)]
              (TableFoot nullAttr [])

      it "wraps output in table:: block" $
        renderOne (mkTable ["Name", "Value"] []) `shouldContainT` "table::"

      it "joins cells in a row with ||" $
        renderOne (mkTable ["A", "B"] []) `shouldContainT` "## A || B"

      it "includes both header and body rows" $ do
        let out = renderOne (mkTable ["header-col"] [["body-col"]])
        out `shouldContainT` "## header-col"
        out `shouldContainT` "## body-col"

    describe "Figure" $ do
      it "single-image figure with caption -> image::url#caption::" $
        renderOne (Figure nullAttr (Caption Nothing [Para [Str "My caption"]])
                    [Plain [Image nullAttr [] ("images/foo.png", "")]])
          `shouldBe` "image::images/foo.png#My caption::"

      it "single-image figure with no caption -> image::url::" $
        renderOne (Figure nullAttr (Caption Nothing [])
                    [Plain [Image nullAttr [] ("images/foo.png", "")]])
          `shouldBe` "image::images/foo.png::"

      it "single-image in Para (not Plain) with caption -> same result" $
        renderOne (Figure nullAttr (Caption Nothing [Para [Str "caption"]])
                    [Para [Image nullAttr [] ("images/foo.png", "")]])
          `shouldBe` "image::images/foo.png#caption::"

      it "non-image figure content falls back to rendering blocks" $ do
        let out = renderOne (Figure nullAttr (Caption Nothing []) [Para [Str "fallback"]])
        out `shouldContainT` "fallback"
        out `shouldNotContainT` "image::"

      it "image with link= attr -> image::url#caption#link::" $
        renderOne (Figure nullAttr (Caption Nothing [Para [Str "cap"]])
                    [Plain [Image ("", [], [("link", "Classes/Foo")]) [] ("images/foo.png", "")]])
          `shouldBe` "image::images/foo.png#cap#Classes/Foo::"

      it "image with link= attr and no caption -> image::url##link::" $
        renderOne (Figure nullAttr (Caption Nothing [])
                    [Plain [Image ("", [], [("link", "Classes/Foo")]) [] ("images/foo.png", "")]])
          `shouldBe` "image::images/foo.png##Classes/Foo::"


  -- -------------------------------------------------------------------------
  describe "renderInline" $ do

    it "SoftBreak renders as a space" $
      renderPara [Str "a", SoftBreak, Str "b"] `shouldBe` "a b"

    it "LineBreak renders as a newline" $
      renderPara [Str "a", LineBreak, Str "b"] `shouldContainT` "a\nb"

    it "Emph -> emphasis:: modal tag" $
      renderPara [Emph [Str "word"]] `shouldContainT` "emphasis::word::"

    it "Strong -> strong:: modal tag" $
      renderPara [Strong [Str "word"]] `shouldContainT` "strong::word::"

    it "Strikeout -> soft:: modal tag" $
      renderPara [Strikeout [Str "word"]] `shouldContainT` "soft::word::"

    it "Code -> code:: modal tag" $
      renderPara [Code nullAttr "foo"] `shouldContainT` "code::foo::"

    it "InlineMath -> math:: modal tag" $
      renderPara [Math InlineMath "x^2"] `shouldContainT` "math::x^2::"

    it "DisplayMath -> math:: block tag" $
      renderPara [Math DisplayMath "x^2"] `shouldContainT` "math::"

    it "Underline passes through as plain text (no SCDoc equivalent)" $ do
      let out = renderPara [Underline [Str "text"]]
      out `shouldContainT` "text"
      out `shouldNotContainT` "underline::"

    it "Superscript passes through as plain text" $ do
      let out = renderPara [Superscript [Str "2"]]
      out `shouldContainT` "2"
      out `shouldNotContainT` "superscript::"

    it "Subscript passes through as plain text" $ do
      let out = renderPara [Subscript [Str "2"]]
      out `shouldContainT` "2"
      out `shouldNotContainT` "subscript::"

    it "SmallCaps passes through as plain text" $ do
      let out = renderPara [SmallCaps [Str "word"]]
      out `shouldContainT` "word"
      out `shouldNotContainT` "smallcaps::"

    it "Cite passes through fallback inlines" $
      renderPara [Cite [Citation "ref" [] [] NormalCitation 0 0] [Str "Author 2024"]]
        `shouldContainT` "Author 2024"

    it "Note -> footnote:: block" $
      renderPara [Note [Para [Str "note text"]]] `shouldContainT` "footnote::"

    it "Span with soft class -> soft:: modal tag" $
      renderPara [Span ("", ["soft"], []) [Str "muted"]]
        `shouldContainT` "soft::muted::"

    it "Span with teletype class -> teletype:: modal tag" $
      renderPara [Span ("", ["teletype"], []) [Str "tt"]]
        `shouldContainT` "teletype::tt::"

    it "Span with unknown class passes through" $
      renderPara [Span ("", ["custom"], []) [Str "text"]]
        `shouldContainT` "text"

    it "Image with alt text -> image::url#alt::" $
      renderPara [Image nullAttr [Str "logo"] ("images/logo.png", "")]
        `shouldBe` "image::images/logo.png#logo::"

    it "Image with no alt text -> image::url::" $
      renderPara [Image nullAttr [] ("images/bare.png", "")]
        `shouldBe` "image::images/bare.png::"

    it "Image strips HelpSource/ prefix from URL" $ do
      let out = renderPara [Image nullAttr [] ("HelpSource/images/foo.png", "")]
      out `shouldContainT` "image::images/foo.png"
      out `shouldNotContainT` "HelpSource"

    it "Image strips ./ prefix from URL" $ do
      let out = renderPara [Image nullAttr [] ("./images/foo.png", "")]
      out `shouldContainT` "image::images/foo.png"
      out `shouldNotContainT` "image::./"

    it "Image with link= attr and alt text -> image::url#alt#link::" $
      renderPara [Image ("", [], [("link", "Classes/Foo")]) [Str "logo"] ("images/logo.png", "")]
        `shouldBe` "image::images/logo.png#logo#Classes/Foo::"

    it "Image with link= attr and no alt -> image::url##link::" $
      renderPara [Image ("", [], [("link", "Classes/Foo")]) [] ("images/logo.png", "")]
        `shouldBe` "image::images/logo.png##Classes/Foo::"

    it "RawInline schelp passes through verbatim" $
      renderPara [RawInline (Format "schelp") "anchor::foo::"]
        `shouldContainT` "anchor::foo::"

    it "RawInline for other formats is dropped" $
      renderPara [RawInline (Format "html") "<b>bold</b>"]
        `shouldBe` ""

    it "Quoted SingleQuote wraps in single quotes" $
      renderPara [Quoted SingleQuote [Str "word"]]
        `shouldContainT` "'word'"

    it "Quoted DoubleQuote wraps in double quotes" $
      renderPara [Quoted DoubleQuote [Str "word"]]
        `shouldContainT` "\"word\""


  -- -------------------------------------------------------------------------
  describe "renderLink" $ do

    it "external URL with label -> url##label" $
      renderPara [Link nullAttr [Str "click here"] ("https://example.com", "")]
        `shouldContainT` "link::https://example.com##click here::"

    it "external URL with redundant label -> url only, no ##" $ do
      let out = renderPara [Link nullAttr [Str "https://example.com"] ("https://example.com", "")]
      out `shouldContainT` "link::https://example.com::"
      out `shouldNotContainT` "##"

    it "internal link with label -> Target##label" $
      renderPara [Link nullAttr [Str "My Synth"] ("MySynth", "")]
        `shouldContainT` "link::MySynth##My Synth::"

    it "internal link with redundant label (matches class name) -> no ##" $
      renderPara [Link nullAttr [Str "MySynth"] ("MySynth", "")]
        `shouldNotContainT` "##"

    it "strips .schelp extension from internal link target" $ do
      let out = renderPara [Link nullAttr [Str "label"] ("MySynth.schelp", "")]
      out `shouldContainT` "link::MySynth"
      out `shouldNotContainT` ".schelp"

    it "strips .md extension from internal link target" $ do
      let out = renderPara [Link nullAttr [Str "label"] ("MySynth.md", "")]
      out `shouldContainT` "link::MySynth"
      out `shouldNotContainT` ".md"

    it "strips HelpSource/ prefix from internal link target" $ do
      let out = renderPara [Link nullAttr [Str "label"] ("HelpSource/Classes/Foo", "")]
      out `shouldContainT` "link::Classes/Foo"
      out `shouldNotContainT` "HelpSource"

    it "strips .html extension from internal link target" $ do
      let out = renderPara [Link nullAttr [Str "label"] ("MySynth.html", "")]
      out `shouldContainT` "link::MySynth"
      out `shouldNotContainT` ".html"

    it "strips ./ prefix from internal link target" $ do
      let out = renderPara [Link nullAttr [Str "label"] ("./Classes/Foo", "")]
      out `shouldContainT` "link::Classes/Foo"
      out `shouldNotContainT` "./"

    it "internal link whose label matches the last path component omits ##" $
      renderPara [Link nullAttr [Str "Foo"] ("Classes/Foo", "")]
        `shouldNotContainT` "##"

    it "anchor-only link (#section) uses # as label separator" $
      renderPara [Link nullAttr [Str "section"] ("#my-section", "")]
        `shouldContainT` "link::#my-section#section::"

    it "internal link with anchor uses # as label separator" $
      renderPara [Link nullAttr [Str "My Label"] ("Foo#bar", "")]
        `shouldContainT` "link::Foo#bar#My Label::"

    it "link with empty URL renders ##label (no target)" $
      renderPara [Link nullAttr [Str "label"] ("", "")]
        `shouldBe` "link::##label::"

    it "link with empty label renders target only (no ##)" $
      renderPara [Link nullAttr [] ("https://example.com", "")]
        `shouldBe` "link::https://example.com::"


  -- -------------------------------------------------------------------------
  describe "method/argument inference" $ do

    it "H2 inside classmethods section is inferred as method::" $ do
      let blocks = [ Header 1 nullAttr [Str "Class Methods"]
                   , Header 2 nullAttr [Str "play"]
                   ]
      writeSCDocPure (doc blocks) `shouldContainT` "method:: play"

    it "H2 inside instancemethods section is inferred as method::" $ do
      let blocks = [ Header 1 nullAttr [Str "Instance Methods"]
                   , Header 2 nullAttr [Str "init"]
                   ]
      writeSCDocPure (doc blocks) `shouldContainT` "method:: init"

    it "H2 outside a method section is not inferred as method::" $ do
      let out = writeSCDocPure (doc [Header 2 nullAttr [Str "play"]])
      out `shouldContainT`    "subsection:: play"
      out `shouldNotContainT` "method::"

    it "H3 under an inferred method is inferred as argument::" $ do
      let blocks = [ Header 1 nullAttr [Str "Class Methods"]
                   , Header 2 nullAttr [Str "ar"]
                   , Header 3 nullAttr [Str "freq"]
                   ]
      writeSCDocPure (doc blocks) `shouldContainT` "argument:: freq"

    it "H3 outside method context is not inferred as argument::" $ do
      let out = writeSCDocPure (doc [Header 3 nullAttr [Str "Notes"]])
      out `shouldContainT`    "subsubsection:: Notes"
      out `shouldNotContainT` "argument::"

    it "Discussion H2 inside method section stays discussion::" $ do
      let blocks = [ Header 1 nullAttr [Str "Class Methods"]
                   , Header 2 nullAttr [Str "Discussion"]
                   ]
          out = writeSCDocPure (doc blocks)
      out `shouldContainT`    "discussion::"
      out `shouldNotContainT` "method:: Discussion"

    it "Returns H3 under a method stays returns::" $ do
      let blocks = [ Header 1 nullAttr [Str "Class Methods"]
                   , Header 2 nullAttr [Str "ar"]
                   , Header 3 nullAttr [Str "Returns"]
                   ]
          out = writeSCDocPure (doc blocks)
      out `shouldContainT`    "returns::"
      out `shouldNotContainT` "argument:: Returns"

    it "explicit .private H2 inside method section is not overridden to method::" $ do
      let blocks = [ Header 1 nullAttr [Str "Class Methods"]
                   , Header 2 ("", ["private"], []) [Str "helper"]
                   ]
          out = writeSCDocPure (doc blocks)
      out `shouldContainT`    "private:: helper"
      out `shouldNotContainT` "method:: helper"

    it "method section ends at the next H1" $ do
      let blocks = [ Header 1 nullAttr [Str "Class Methods"]
                   , Header 2 nullAttr [Str "ar"]
                   , Header 1 nullAttr [Str "Examples"]
                   , Header 2 nullAttr [Str "Usage"]
                   ]
          out = writeSCDocPure (doc blocks)
      out `shouldContainT`    "method:: ar"
      out `shouldContainT`    "subsection:: Usage"
      out `shouldNotContainT` "method:: Usage"


  -- -------------------------------------------------------------------------
  describe "anchor injection" $ do

    it "inserts anchor:: after a heading whose ID is referenced by a local link" $ do
      let blocks =
            [ Header 2 ("my-section", [], []) [Str "My Section"]
            , Para [Link nullAttr [Str "go"] ("#my-section", "")]
            ]
      writeSCDocPure (doc blocks) `shouldContainT` "anchor::my-section::"

    it "does not insert anchor:: for a heading whose ID is not referenced" $
      writeSCDocPure (doc [Header 2 ("unused-id", [], []) [Str "Unused"]])
        `shouldNotContainT` "anchor::"

    it "respects the no-anchor class" $ do
      let blocks =
            [ Header 2 ("sec", ["no-anchor"], []) [Str "Section"]
            , Para [Link nullAttr [Str "link"] ("#sec", "")]
            ]
      writeSCDocPure (doc blocks) `shouldNotContainT` "anchor::"

    it "anchor:: appears after the heading line" $ do
      let blocks =
            [ Header 2 ("s", [], []) [Str "Sec"]
            , Para [Link nullAttr [Str "x"] ("#s", "")]
            ]
          out           = writeSCDocPure (doc blocks)
          (prePart, _)  = T.breakOn "anchor::s::" out
      prePart `shouldContainT` "subsection:: Sec"


  -- -------------------------------------------------------------------------
  describe "escaping" $ do

    it "escapes :: inside a modal tag (inline)" $
      renderPara [Code nullAttr "a::b"] `shouldContainT` "code::a\\::b::"

    it "escapes a standalone :: line inside a code block" $
      renderOne (CodeBlock nullAttr "::") `shouldContainT` "\\::"

    it "escapes :: surrounded only by whitespace inside a code block" $
      renderOne (CodeBlock nullAttr "  ::  ") `shouldContainT` "\\::"

    it "does NOT escape :: that appears mid-line inside a code block" $ do
      let out = renderOne (CodeBlock nullAttr "foo :: bar")
      out `shouldContainT` "foo :: bar"
      out `shouldNotContainT` "foo \\:: bar"

  describe "writeSCDoc template" $ do

    it "returns plain render when no template is set" $ do
      result <- Pandoc.runIO $ writeSCDoc def (doc [Para [Str "hello"]])
      case result of
        Left  e -> fail (show e)
        Right t -> t `shouldContainT` "hello"

    it "applies writerTemplate wrapping the body" $ do
      tpl  <- either fail pure =<< DT.compileTemplate "" "BEFORE\n$body$\nAFTER"
      result <- Pandoc.runIO $
        writeSCDoc def { writerTemplate = Just tpl } (doc [Para [Str "hello"]])
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "BEFORE"
          t `shouldContainT` "hello"
          t `shouldContainT` "AFTER"

    it "template body contains the full rendered SCDoc output" $ do
      tpl  <- either fail pure =<< DT.compileTemplate "" "$body$"
      result <- Pandoc.runIO $
        writeSCDoc def { writerTemplate = Just tpl }
          (docWithMeta [("title", MetaString "MyClass")] [])
      case result of
        Left  e -> fail (show e)
        Right t -> t `shouldContainT` "title:: MyClass"

    it "document metadata is available as template variables ($title$)" $ do
      tpl  <- either fail pure =<< DT.compileTemplate "" "$title$"
      result <- Pandoc.runIO $
        writeSCDoc def { writerTemplate = Just tpl }
          (docWithMeta [("title", MetaString "MySynth")] [])
      case result of
        Left  e -> fail (show e)
        Right t -> t `shouldContainT` "MySynth"

    it "writerVariables are available as template variables" $ do
      tpl  <- either fail pure =<< DT.compileTemplate "" "$myvar$"
      let opts = def { writerTemplate  = Just tpl
                     , writerVariables = defField "myvar" ("hello" :: Text) mempty }
      result <- Pandoc.runIO $ writeSCDoc opts (doc [])
      case result of
        Left  e -> fail (show e)
        Right t -> t `shouldContainT` "hello"


  -- -------------------------------------------------------------------------
  -- Format compatibility: verify that the RST and HTML readers produce the
  -- same AST constructs our writer handles, exercising the full pipeline.

  describe "Markdown reader compatibility" $ do

    it "Markdown Class.method copymethod heading renders copymethod::" $ do
      let src = "## SinOsc.ar {.copymethod}\n\nCopied.\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readMarkdown mdReaderOpts src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "copymethod:: SinOsc ar"
          t `shouldNotContainT` "method:: SinOsc.ar"

    it "Markdown Class method copymethod heading renders copymethod::" $ do
      let src = "## SinOsc ar {.copymethod}\n\nCopied.\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readMarkdown mdReaderOpts src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "copymethod:: SinOsc ar"
          t `shouldNotContainT` "subsection:: SinOsc ar"

    it "Markdown source+method attrs render copymethod::" $ do
      let src = "## copy {.copymethod source=\"SinOsc\" method=\"ar\"}\n\nCopied.\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readMarkdown mdReaderOpts src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "copymethod:: SinOsc ar"
          t `shouldNotContainT` "method:: copy"

  describe "RST reader compatibility" $ do

    it "RST headings infer method:: and argument:: inside a method section" $ do
      let src = "Class Methods\n=============\n\nar, kr\n------\n\nGenerate.\n\nfreq\n~~~~\n\nHz.\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "classmethods::"
          t `shouldContainT` "method:: ar, kr"
          t `shouldContainT` "argument:: freq"

    it "RST Returns heading under a method becomes returns::" $ do
      let src = "Class Methods\n=============\n\nar\n--\n\nReturns\n~~~~~~~\n\nA UGen.\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "returns::"
          t `shouldNotContainT` "argument:: Returns"

    it "RST .. warning:: directive maps to warning:: without generated title" $ do
      let src = ".. warning::\n\n   Dangerous!\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "warning::"
          t `shouldNotContainT` "warning::\nWarning"

    it "RST .. note:: directive maps to note:: without generated title" $ do
      let src = ".. note::\n\n   Important!\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "note::"
          t `shouldNotContainT` "note::\nNote"

    it "RST .. math:: directive maps to math:: block" $ do
      let src = ".. math::\n\n   \\int_0^\\infty f(x)\\,dx\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "math::"
          t `shouldContainT` "\\int_0^\\infty f(x)\\,dx"

    it "RST simple table maps to table:: block" $ do
      let src = "======  =====\nName    Value\n======  =====\nfreq    440\n======  =====\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "table::"
          t `shouldContainT` "## Name || Value"
          t `shouldContainT` "## freq || 440"

    it "RST .. raw:: schelp passes through verbatim" $ do
      let src = ".. raw:: schelp\n\n   anchor::foo::\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> t `shouldContainT` "anchor::foo::"

    it "RST nested list items are flattened to sibling ## entries" $ do
      let src = "- outer\n\n  - inner a\n  - inner b\n\n- last\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "## outer"
          t `shouldContainT` "## inner a"
          t `shouldContainT` "## inner b"
          t `shouldContainT` "## last"

    it "RST code-block maps to code:: block" $ do
      let src = ".. code-block::\n\n   { SinOsc.ar(440) }.play\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "code::"
          t `shouldContainT` "{ SinOsc.ar(440) }.play"

    it "RST code-block with teletype language maps to teletype:: block" $ do
      let src = ".. code-block:: teletype\n\n   sclang -e 'Server.default.boot'\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "teletype::"
          t `shouldContainT` "sclang -e 'Server.default.boot'"

    it "RST definition list maps to definitionlist::" $ do
      let src = "Term A\n   Definition for A.\n\nTerm B\n   Definition for B.\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "definitionlist::"
          t `shouldContainT` "## Term A"
          t `shouldContainT` "|| Definition for A."
          t `shouldContainT` "## Term B"

    it "RST figure with caption maps to image::url#caption::" $ do
      let src = ".. figure:: images/foo.png\n\n   A figure caption.\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "image::images/foo.png"
          t `shouldContainT` "A figure caption"


  -- -------------------------------------------------------------------------
  describe "extractDocInfoMeta" $ do

    it "lifts title from a leading DefinitionList into Meta" $ do
      let defs = [([Str "title"], [[Para [Str "MyClass"]]])]
          (meta, body) = extractDocInfoMeta nullMeta [DefinitionList defs]
      lookupMeta "title" meta `shouldBe` Just (MetaString "MyClass")
      body `shouldBe` []

    it "lifts summary from a leading DefinitionList" $ do
      let defs = [([Str "summary"], [[Para [Str "A resonant filter"]]])]
          (meta, _) = extractDocInfoMeta nullMeta [DefinitionList defs]
      lookupMeta "summary" meta `shouldBe` Just (MetaString "A resonant filter")

    it "leaves unknown keys in body as a DefinitionList" $ do
      let defs = [([Str "author"], [[Para [Str "Alice"]]])]
          (meta, body) = extractDocInfoMeta nullMeta [DefinitionList defs]
      lookupMeta "author" meta `shouldBe` Nothing
      body `shouldBe` [DefinitionList defs]

    it "splits recognized from unknown, keeping unknown in body" $ do
      let defs = [ ([Str "title"],  [[Para [Str "MyClass"]]])
                 , ([Str "author"], [[Para [Str "Alice"]]])
                 ]
          (meta, body) = extractDocInfoMeta nullMeta [DefinitionList defs]
      lookupMeta "title" meta `shouldBe` Just (MetaString "MyClass")
      body `shouldBe` [DefinitionList [([Str "author"], [[Para [Str "Alice"]]])]]

    it "existing Meta entries take precedence over docinfo" $ do
      let existingMeta = Meta (Map.fromList [("title", MetaString "Existing")])
          defs = [([Str "title"], [[Para [Str "FromDocInfo"]]])]
          (meta, _) = extractDocInfoMeta existingMeta [DefinitionList defs]
      lookupMeta "title" meta `shouldBe` Just (MetaString "Existing")

    it "keyword field becomes a MetaList" $ do
      let defs = [([Str "keyword"], [[Para [Str "oscillator"]]])]
          (meta, _) = extractDocInfoMeta nullMeta [DefinitionList defs]
      lookupMeta "keyword" meta `shouldBe` Just (MetaList [MetaString "oscillator"])

    it "repeated keyword entries accumulate into a MetaList in document order" $ do
      let defs = [ ([Str "keyword"], [[Para [Str "oscillator"]]])
                 , ([Str "keyword"], [[Para [Str "test"]]])
                 ]
          (meta, _) = extractDocInfoMeta nullMeta [DefinitionList defs]
      lookupMeta "keyword" meta `shouldBe`
        Just (MetaList [MetaString "oscillator", MetaString "test"])

    it "does not produce SCDoc link syntax in plain-text extraction" $ do
      let defs = [([Str "summary"],
                   [[Para [Str "See", Space,
                           Link nullAttr [Str "SinOsc"] ("Classes/SinOsc", ""),
                           Str "."]]])]
          (meta, _) = extractDocInfoMeta nullMeta [DefinitionList defs]
      case lookupMeta "summary" meta of
        Just (MetaString t) -> do
          t `shouldContainT` "SinOsc"
          t `shouldNotContainT` "link::"
        other -> expectationFailure ("expected MetaString, got " <> show other)

    it "does not modify blocks when no recognized field is present" $ do
      let blocks = [Para [Str "intro"]]
          (meta, body) = extractDocInfoMeta nullMeta blocks
      meta `shouldBe` nullMeta
      body `shouldBe` blocks

    it "RST docinfo: custom fields arrive as DefinitionList and are lifted" $ do
      let src = ":title: MyClass\n:summary: A UGen.\n\nSome text.\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "title:: MyClass"
          t `shouldContainT` "summary:: A UGen."

    it "RST docinfo: link in summary becomes plain text, not SCDoc link syntax" $ do
      let src = ":summary: See `SinOsc <Classes/SinOsc>`__.\n\nBody.\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readRST Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "summary:: See SinOsc."
          t `shouldNotContainT` "link::"


  -- -------------------------------------------------------------------------
  describe "extractLeadingOrgMeta" $ do

    let orgRaw = RawBlock (Format "org")

    it "lifts #+SUMMARY from a leading org raw block into Meta" $ do
      let (meta, body) = extractLeadingOrgMeta nullMeta [orgRaw "#+SUMMARY: A resonant filter"]
      lookupMeta "summary" meta `shouldBe` Just (MetaString "A resonant filter")
      body `shouldBe` []

    it "lifts #+CATEGORIES from a leading org raw block" $ do
      let (meta, _) = extractLeadingOrgMeta nullMeta [orgRaw "#+CATEGORIES: Reference>Tests"]
      lookupMeta "categories" meta `shouldBe` Just (MetaString "Reference>Tests")

    it "lifts #+RELATED from a leading org raw block" $ do
      let (meta, _) = extractLeadingOrgMeta nullMeta [orgRaw "#+RELATED: Classes/SinOsc"]
      lookupMeta "related" meta `shouldBe` Just (MetaString "Classes/SinOsc")

    it "#+KEYWORD becomes a MetaList" $ do
      let (meta, _) = extractLeadingOrgMeta nullMeta [orgRaw "#+KEYWORD: oscillator"]
      lookupMeta "keyword" meta `shouldBe` Just (MetaList [MetaString "oscillator"])

    it "repeated #+KEYWORD entries accumulate into a MetaList in document order" $ do
      let blocks = [orgRaw "#+KEYWORD: oscillator", orgRaw "#+KEYWORD: test"]
          (meta, _) = extractLeadingOrgMeta nullMeta blocks
      lookupMeta "keyword" meta `shouldBe`
        Just (MetaList [MetaString "oscillator", MetaString "test"])

    it "existing Meta entries take precedence over org keywords" $ do
      let existingMeta = Meta (Map.fromList [("summary", MetaString "Existing")])
          (meta, _) = extractLeadingOrgMeta existingMeta [orgRaw "#+SUMMARY: FromOrg"]
      lookupMeta "summary" meta `shouldBe` Just (MetaString "Existing")

    it "unrecognized org keywords are consumed but not added to Meta" $ do
      let (meta, body) = extractLeadingOrgMeta nullMeta [orgRaw "#+AUTHOR: Alice"]
      lookupMeta "author" meta `shouldBe` Nothing
      body `shouldBe` []

    it "does not modify blocks when no leading org raw block is present" $ do
      let blocks = [Para [Str "intro"]]
          (meta, body) = extractLeadingOrgMeta nullMeta blocks
      meta `shouldBe` nullMeta
      body `shouldBe` blocks

    it "stops consuming at the first non-org-raw block" $ do
      let blocks = [ orgRaw "#+SUMMARY: first"
                   , Para [Str "content"]
                   , orgRaw "#+SUMMARY: late"
                   ]
          (meta, body) = extractLeadingOrgMeta nullMeta blocks
      lookupMeta "summary" meta `shouldBe` Just (MetaString "first")
      body `shouldBe` [Para [Str "content"], orgRaw "#+SUMMARY: late"]

    -- Validate that the real Pandoc org reader actually emits the
    -- RawBlock (Format "org") structure that extractLeadingOrgMeta expects.
    -- If readOrg ever changes how it represents unrecognised #+KEY: lines,
    -- these tests will fail before the hand-crafted-AST tests above notice.
    describe "with real readOrg output" $ do

      it "readOrg delivers #+SUMMARY as a RawBlock that extractLeadingOrgMeta can lift" $ do
        let src = "#+SUMMARY: A brief summary.\n\ntext\n" :: Text
        result <- Pandoc.runIO $ Pandoc.readOrg Pandoc.def src
        case result of
          Left  e -> fail (show e)
          Right (Pandoc _ blocks) ->
            lookupMeta "summary" (fst (extractLeadingOrgMeta nullMeta blocks))
              `shouldBe` Just (MetaString "A brief summary.")

      it "readOrg delivers #+CATEGORIES as a RawBlock that extractLeadingOrgMeta can lift" $ do
        let src = "#+CATEGORIES: Reference>Tests\n\ntext\n" :: Text
        result <- Pandoc.runIO $ Pandoc.readOrg Pandoc.def src
        case result of
          Left  e -> fail (show e)
          Right (Pandoc _ blocks) ->
            lookupMeta "categories" (fst (extractLeadingOrgMeta nullMeta blocks))
              `shouldBe` Just (MetaString "Reference>Tests")

      it "readOrg delivers repeated #+KEYWORD as RawBlocks that accumulate" $ do
        let src = "#+KEYWORD: oscillator\n#+KEYWORD: test\n\ntext\n" :: Text
        result <- Pandoc.runIO $ Pandoc.readOrg Pandoc.def src
        case result of
          Left  e -> fail (show e)
          Right (Pandoc _ blocks) ->
            lookupMeta "keyword" (fst (extractLeadingOrgMeta nullMeta blocks))
              `shouldBe` Just (MetaList [MetaString "oscillator", MetaString "test"])

      it "readOrg delivers #+REDIRECT as a RawBlock that extractLeadingOrgMeta can lift" $ do
        let src = "#+REDIRECT: Classes/OldName\n\ntext\n" :: Text
        result <- Pandoc.runIO $ Pandoc.readOrg Pandoc.def src
        case result of
          Left  e -> fail (show e)
          Right (Pandoc _ blocks) ->
            lookupMeta "redirect" (fst (extractLeadingOrgMeta nullMeta blocks))
              `shouldBe` Just (MetaString "Classes/OldName")

      it "readOrg puts #+TITLE into Meta directly (not into blocks)" $ do
        let src = "#+TITLE: My Class\n\ntext\n" :: Text
        result <- Pandoc.runIO $ Pandoc.readOrg Pandoc.def src
        case result of
          Left  e -> fail (show e)
          Right (Pandoc orgMeta _) ->
            lookupMeta "title" orgMeta `shouldNotBe` Nothing


  -- -------------------------------------------------------------------------
  describe "org reader compatibility" $ do

    it "org headings infer method:: and argument:: inside a method section" $ do
      let src = "* Class Methods\n\n** ar, kr\n\nGenerate.\n\n*** freq\n\nHz.\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "classmethods::"
          t `shouldContainT` "method:: ar, kr"
          t `shouldContainT` "argument:: freq"

    it "org metadata keywords are lifted into the schelp header" $ do
      let src = T.unlines
                  [ "#+SUMMARY: A brief summary."
                  , "#+CATEGORIES: Reference>Tests"
                  , "#+KEYWORD: oscillator"
                  , ""
                  , "* Description"
                  , ""
                  , "text"
                  ]
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "summary:: A brief summary."
          t `shouldContainT` "categories:: Reference>Tests"
          t `shouldContainT` "keyword:: oscillator"

    it "org #+BEGIN_note maps to note:: without a generated title" $ do
      let src = "#+BEGIN_note\nImportant!\n#+END_note\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "note::"
          t `shouldNotContainT` "note::\nNote"

    it "org #+BEGIN_EXPORT schelp passes through verbatim" $ do
      let src = "#+BEGIN_EXPORT schelp\nanchor::foo::\n#+END_EXPORT\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> t `shouldContainT` "anchor::foo::"

    it "#+KEYWORDS: a, b (plural) emits two keyword:: entries after comma-split" $ do
      let src = T.unlines
                  [ "#+KEYWORDS: oscillator, audio"
                  , ""
                  , "* Description"
                  , ""
                  , "text"
                  ]
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "keyword:: oscillator"
          t `shouldContainT` "keyword:: audio"

    it "org internal link with CUSTOM_ID property emits anchor::" $ do
      let src = T.unlines
                  [ "An [[#my-section][link]]."
                  , ""
                  , "* My Section"
                  , ":PROPERTIES:"
                  , ":CUSTOM_ID: my-section"
                  , ":END:"
                  , ""
                  , "text"
                  ]
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> t `shouldContainT` "anchor::my-section::"

    it "org #+BEGIN_warning maps to warning:: without a generated title" $ do
      let src = "#+BEGIN_warning\nDangerous!\n#+END_warning\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "warning::"
          t `shouldNotContainT` "warning::\nWarning"

    it "org #+BEGIN_SRC maps to code:: block" $ do
      let src = "#+BEGIN_SRC\n{ SinOsc.ar(440) }.play\n#+END_SRC\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "code::"
          t `shouldContainT` "{ SinOsc.ar(440) }.play"

    it "org #+BEGIN_SRC teletype maps to teletype:: block" $ do
      let src = "#+BEGIN_SRC teletype\nsclang -e 'Server.default.boot'\n#+END_SRC\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "teletype::"
          t `shouldContainT` "sclang -e 'Server.default.boot'"

    it "org definition list maps to definitionlist::" $ do
      let src = "- Term A :: Definition for A.\n- Term B :: Definition for B.\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "definitionlist::"
          t `shouldContainT` "## Term A"
          t `shouldContainT` "|| Definition for A."
          t `shouldContainT` "## Term B"

    it "org table maps to table:: block" $ do
      let src = "| Name | Value |\n|------+-------|\n| freq |   440 |\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "table::"
          t `shouldContainT` "## Name || Value"
          t `shouldContainT` "## freq || 440"

    it "#+REDIRECT: is lifted into the schelp header via extractLeadingOrgMeta" $ do
      let src = T.unlines
                  [ "#+REDIRECT: Classes/OldName"
                  , ""
                  , "* Description"
                  , ""
                  , "text"
                  ]
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> t `shouldContainT` "redirect:: Classes/OldName"

    it "org Returns heading under a method becomes returns::" $ do
      let src = "* Class Methods\n\n** ar\n\n*** Returns\n\nA UGen.\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "returns::"
          t `shouldNotContainT` "argument:: Returns"

    it "org image with #+CAPTION maps to image::url#caption::" $ do
      let src = "#+CAPTION: A waveform\n[[file:images/waveform.png]]\n" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readOrg Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "image::images/waveform.png"
          t `shouldContainT` "A waveform"


  describe "HTML reader compatibility" $ do
    -- Note: <div class="warning"> → warning:: is tested at the AST level above
    -- ("warning class -> warning:: range block").  The HTML reader requires the
    -- native_divs extension (enabled by default in the CLI via getReader) to
    -- preserve div classes; Pandoc.def intentionally uses no extensions here.

    it "HTML headings infer method:: inside a method section" $ do
      let src = "<h1>Class Methods</h1><h2>ar</h2><p>Generate.</p>" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readHtml Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "classmethods::"
          t `shouldContainT` "method:: ar"

    it "HTML nested list items are flattened to sibling ## entries" $ do
      let src = "<ul><li>outer<ul><li>inner a</li><li>inner b</li></ul></li><li>last</li></ul>" :: Text
      result <- Pandoc.runIO $ writeSCDocPure <$> Pandoc.readHtml Pandoc.def src
      case result of
        Left  e -> fail (show e)
        Right t -> do
          t `shouldContainT` "## outer"
          t `shouldContainT` "## inner a"
          t `shouldContainT` "## inner b"
          t `shouldContainT` "## last"
