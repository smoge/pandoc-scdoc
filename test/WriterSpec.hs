{-# LANGUAGE OverloadedStrings #-}

module WriterSpec (spec) where

import           Test.Hspec

import           Text.Pandoc.Definition
import           Text.Pandoc.Readers.SCDoc  (readSCDocPure)
import           Text.Pandoc.Writers.SCDoc  (writeSCDocPure)

import           TestSupport


spec :: Spec
spec = do

  describe "renderBlock" $ do

    describe "Para / Plain" $ do
      it "renders Plain the same as Para" $
        renderOne (Plain [Str "hello"]) `shouldBe` "hello"

    describe "CodeBlock" $ do
      it "passes scdoc code through verbatim" $
        renderOne (CodeBlock ("", ["scdoc"], []) "section:: A")
          `shouldBe` "section:: A"

    describe "BulletList" $ do
      it "preserves nested OrderedList as an inner numberedlist:: block" $ do
        let out = renderOne (BulletList
                    [ [Para [Str "top"], OrderedList (1, DefaultStyle, DefaultDelim)
                        [[Para [Str "sub 1"]], [Para [Str "sub 2"]]]]
                    ])
        out `shouldContainT`    "## top\n\nnumberedlist::\n## sub 1\n## sub 2\n::"
        out `shouldNotContainT` "## top\n## sub 1"

      -- Mixed items keep trailing blocks under the same @##@.
      it "preserves trailing block after a nested list (mixed shape)" $ do
        let out = renderOne (BulletList
                    [ [ Para [Str "intro"]
                      , BulletList [[Para [Str "inner"]]]
                      , Para [Str "outro"]
                      ]
                    ])
        out `shouldContainT`
          "## intro\n\nlist::\n## inner\n::\n\noutro"
        out `shouldNotContainT` "## outro"

      -- Multi-block item bodies stay under one @##@.
      it "multi-block list item stays under one ## with blank-line separation" $ do
        let out = renderOne (BulletList
                    [ [Para [Str "intro"], CodeBlock nullAttr "x = 1;"]
                    ])
        -- Code block follows inside the same item.
        out `shouldContainT` "## intro\n\ncode::\nx = 1;\n::"
        -- No extra @##@ before the code block.
        out `shouldNotContainT` "## code::"

    describe "OrderedList" $ do
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
      it "level 1 'ClassMethods' -> classmethods::" $
        renderOne (Header 1 nullAttr [Str "ClassMethods"])
          `shouldContainT` "classmethods::"

      it "level 4 and above -> subsubsection::" $
        renderOne (Header 4 nullAttr [Str "Deep"])
          `shouldContainT` "subsubsection:: Deep"

    describe "SCDoc-class Header" $ do
      it "single-character _ name is left unchanged (length guard)" $
        renderOne (Header 2 ("", ["method"], []) [Str "_"])
          `shouldContainT` "method:: _"

      it "method class with args= attr appends argument list" $
        renderOne (Header 2 ("", ["method"], [("args", "freq=440, amp=0.1")]) [Str "ar"])
          `shouldBe` "method:: ar freq=440, amp=0.1"

      it "returns class with text= attr includes inline description" $
        renderOne (Header 3 ("", ["returns"], [("text", "a UGen")]) [])
          `shouldBe` "returns:: a UGen"

      it "heading text Returns with text= attr -> returns:: with description" $
        renderOne (Header 2 ("", [], [("text", "a UGen")]) [Str "Returns"])
          `shouldBe` "returns:: a UGen"

      it "heading text Warning is case-insensitive and wraps only the next block" $ do
        let out = writeSCDocPure (doc [ Header 2 nullAttr [Str "WARNING"]
                                      , Para [Str "danger"]
                                      , Para [Str "safe"]
                                      , Header 2 nullAttr [Str "Details"]
                                      , Para [Str "done"]
                                      ])
        out `shouldContainT` "warning::\ndanger\n::"
        out `shouldContainT` "warning::\ndanger\n::\n\nsafe"
        out `shouldContainT` "subsection:: Details"
        out `shouldContainT` "done"
        out `shouldNotContainT` "subsection:: WARNING"

      -- Structural headings named warning are declarations, not admonitions.
      it "method:: heading named 'warning' is NOT rewritten to a warning directive" $ do
        let out = renderOne (Header 2 ("",["method","imethod"],[]) [Str "warning"])
        out `shouldContainT`    "method:: warning"
        out `shouldNotContainT` "warning::"

      it "private:: heading named 'warning' is NOT rewritten either" $ do
        let out = renderOne (Header 2 ("",["private","iprivate"],[]) [Str "warning"])
        out `shouldContainT`    "private:: warning"
        out `shouldNotContainT` "warning::"

      -- @no-anchor@ alone should not block warning-heading rewrite.
      it "Header 2 [.no-anchor] named 'warning' still becomes warning:: directive" $ do
        let out = writeSCDocPure (doc [ Header 2 ("",["no-anchor"],[]) [Str "warning"]
                                      , Para [Str "danger"]
                                      ])
        out `shouldContainT`    "warning::\ndanger\n::"
        out `shouldNotContainT` "subsection:: warning"

      -- @anchor@ is structural and must block warning-heading rewrite.
      it "Header 2 [.anchor] named 'warning' stays as anchor::warning::" $ do
        let out = renderOne (Header 2 ("",["anchor"],[]) [Str "warning"])
        out `shouldContainT`    "anchor::warning::"
        out `shouldNotContainT` "warning::\n"

      it "copymethod class with class+method attrs -> copymethod::" $
        renderOne (Header 2 ("", ["copymethod"], [("class", "SinOsc"), ("method", "ar")]) [])
          `shouldBe` "copymethod:: SinOsc ar"

      it "copymethod class with source+method attrs -> copymethod::" $
        renderOne (Header 2 ("", ["copymethod"], [("source", "SinOsc"), ("method", "ar")]) [])
          `shouldBe` "copymethod:: SinOsc ar"

      it "copymethod class with Class.method heading -> copymethod::" $
        renderOne (Header 2 ("", ["copymethod"], []) [Str "SinOsc.ar"])
          `shouldBe` "copymethod:: SinOsc ar"

      -- Class plus multi-word method tail.
      it "copymethod class with 3-word heading -> copymethod:: Class rest" $ do
        let out = renderOne (Header 2 ("", ["copymethod"], [])
                              [Str "SinOsc", Space, Str "ar", Space, Str "extra"])
        out `shouldContainT` "copymethod:: SinOsc ar extra"

      -- Class plus comma-separated method list.
      it "copymethod with operator+comma method list keeps copymethod::" $
        renderOne (Header 2 ("", ["copymethod"], [])
                    [Str "BusPlug", Space, Str "-ar", Str ",",
                     Space, Str "kr"])
          `shouldBe` "copymethod:: BusPlug -ar, kr"

      -- Unsplittable labels keep the literal copymethod:: form.
      it "copymethod with single unparseable word stays as copymethod::" $
        renderOne (Header 2 ("", ["copymethod"], []) [Str "lone"])
          `shouldBe` "copymethod:: lone"

      -- private:: uses comma-list parsing, so setter underscores stay.
      it "private:: preserves trailing underscores on setter names" $
        renderOne (Header 2 ("", ["private"], [])
                    [Str "init, getInfo, info_, prDeviceClosed"])
          `shouldContainT` "private:: init, getInfo, info_, prDeviceClosed"

      -- method:: still strips setter underscores.
      it "method:: still strips trailing underscore on setter name" $ do
        let out = renderOne (Header 2 ("", ["method"], []) [Str "info_"])
        out `shouldContainT` "method:: info"
        out `shouldNotContainT` "method:: info_"

    describe "Div" $ do
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
      it "scdoc raw block passes through verbatim" $
        renderOne (RawBlock (Format "scdoc") "section:: A")
          `shouldContainT` "section:: A"

      it "non-SCDoc raw block is dropped" $
        renderOne (RawBlock (Format "html") "<b>bold</b>")
          `shouldBe` ""

    describe "DefinitionList" $ do
      -- Empty definitions still emit the required @||@ row.
      it "term with no definition emits ## term and || (upstream-valid)" $ do
        let out = renderOne (DefinitionList [([Str "bare"], [])])
        out `shouldContainT` "## bare"
        out `shouldContainT` "||"

      -- Multi-paragraph definition bodies keep paragraph breaks.
      it "multi-paragraph def body keeps the paragraph break" $ do
        let out = renderOne (DefinitionList
                    [([Str "term"], [[Para [Str "p1"], Para [Str "p2"]]])])
        out `shouldContainT` "## term"
        out `shouldContainT` "|| p1\n\np2"

      -- Shared-body terms emit multiple @##@ lines and one @||@ body.
      it "scdoc-defterm Span group emits multi-## + single ||" $ do
        let term = [ Span ("", ["scdoc-defterm"], []) [Str "one"]
                   , LineBreak
                   , Span ("", ["scdoc-defterm"], []) [Str "two"]
                   ]
            out = renderOne (DefinitionList [(term, [[Para [Str "body"]]])])
        out `shouldContainT` "## one\n## two\n|| body"

    describe "Table" $ do
      -- Non-simple cells use block layout.
      let blockCell bs = Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) bs
          oneRowTable cells =
            Table nullAttr (Caption Nothing []) []
              (TableHead nullAttr [])
              [TableBody nullAttr (RowHeadColumns 0) [] [Row nullAttr cells]]
              (TableFoot nullAttr [])

      it "preserves a Para+CodeBlock cell as block layout" $ do
        let out = renderOne $ oneRowTable
              [ blockCell [Para [Str "intro"], CodeBlock nullAttr "x = 1;"]
              , blockCell [Para [Str "other"]]
              ]
        out `shouldContainT` "##\nintro\n\ncode::\nx = 1;\n::\n||\nother\n"
        out `shouldNotContainT` "## intro"   -- not inline form
        out `shouldNotContainT` "x = 1; ||"  -- not collapsed onto the || line

      it "preserves a nested BulletList cell as block layout" $ do
        let out = renderOne $ oneRowTable
              [ blockCell [Para [Str "label"]]
              , blockCell [BulletList [[Para [Str "a"]], [Para [Str "b"]]]]
              ]
        out `shouldContainT` "##\nlabel\n||\nlist::\n## a\n## b\n::\n"

      it "preserves a multi-paragraph cell as block layout" $ do
        let out = renderOne $ oneRowTable
              [ blockCell [Para [Str "p1"], Para [Str "p2"]]
              , blockCell [Para [Str "right"]]
              ]
        out `shouldContainT` "##\np1\n\np2\n||\nright\n"

      it "all-simple rows still use the compact inline form" $ do
        let out = renderOne $ oneRowTable
              [ blockCell [Plain [Str "a"]]
              , blockCell [Para  [Str "b"]]
              ]
        out `shouldContainT`    "## a || b"
        out `shouldNotContainT` "##\na"

      -- Newline-producing inline content must force block layout.
      it "Para with LineBreak inside a Span forces block layout" $ do
        let out = renderOne $ oneRowTable
              [ blockCell [Para [Span nullAttr [Str "a", LineBreak, Str "b"]]]
              , blockCell [Para  [Str "c"]]
              ]
        out `shouldContainT`    "##\na\nb\n||\nc\n"
        out `shouldNotContainT` "## a b || c"

      it "Para with Math DisplayMath forces block layout" $ do
        let out = renderOne $ oneRowTable
              [ blockCell [Para [Math DisplayMath "x^2"]]
              , blockCell [Para [Str "c"]]
              ]
        out `shouldContainT`    "##\n"
        out `shouldContainT`    "math::\nx^2\n::"
        out `shouldContainT`    "||\nc\n"
        out `shouldNotContainT` "## math:: x^2 :: || c"

      -- Full header/body round-trip for block-level cells.
      it "headed table with block-level body cells round-trips" $ do
        let headRow = Row nullAttr
              [ blockCell [Para [Str "name"]]
              , blockCell [Para [Str "value"]]
              ]
            bodyRow = Row nullAttr
              [ blockCell [Para [Str "intro"], CodeBlock nullAttr "x = 1;"]
              , blockCell [BulletList [[Para [Str "a"]], [Para [Str "b"]]]]
              ]
            tbl = Table nullAttr (Caption Nothing []) []
                    (TableHead nullAttr [headRow])
                    [TableBody nullAttr (RowHeadColumns 0) [] [bodyRow]]
                    (TableFoot nullAttr [])
            pd = doc [tbl]
        case readSCDocPure (writeSCDocPure pd) of
          Left  e   -> expectationFailure (show e)
          Right pd' -> pd' `shouldBe` pd

    describe "Figure" $ do
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

    -- Literal @::@ in plain text must be escaped.
    it "Str containing :: gets escaped to \\::" $
      renderPara [Str "method", Str "::"] `shouldContainT` "method\\::"

    it "Str whose full content is :: gets escaped to \\::" $
      renderPara [Str "::"] `shouldContainT` "\\::"

    -- Closing separator avoids @\\::@ merging.
    it "Span teletype with body \\ keeps a separator before close" $ do
      let out = renderPara [Span ("", ["teletype"], []) [Str "\\"]]
      out `shouldContainT` "teletype::\\ ::"

    -- Opening separator avoids leading-colon merging.
    it "Span teletype with body starting with : keeps a separator after open" $ do
      let out = renderPara [Span ("", ["teletype"], []) [Str ":foo"]]
      out `shouldContainT` "teletype:: :foo::"

    -- Closing separator avoids trailing-colon merging.
    it "Strong with body ending in : keeps a separator before close" $ do
      let out = renderPara [Strong [Str "Breaking", Space, Str "change:"]]
      out `shouldContainT` "strong::Breaking change: ::"
      out `shouldNotContainT` "strong::Breaking change:::"

    it "LineBreak renders as a newline" $
      renderPara [Str "a", LineBreak, Str "b"] `shouldContainT` "a\nb"

    it "unsupported inline forms pass through fallback content" $ do
      let underline = renderPara [Underline [Str "text"]]
          sup       = renderPara [Superscript [Str "2"]]
          sub       = renderPara [Subscript [Str "2"]]
          caps      = renderPara [SmallCaps [Str "word"]]
          cite      = renderPara
                        [Cite [Citation "ref" [] [] NormalCitation 0 0]
                          [Str "Author 2024"]]
      underline `shouldContainT` "text"
      underline `shouldNotContainT` "underline::"
      sup `shouldContainT` "2"
      sub `shouldContainT` "2"
      caps `shouldContainT` "word"
      cite `shouldContainT` "Author 2024"

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

    it "RawInline for other formats is dropped" $
      renderPara [RawInline (Format "html") "<b>bold</b>"]
        `shouldBe` ""


  -- -------------------------------------------------------------------------
  describe "renderLink" $ do

    it "external URL with redundant label -> url only, no ##" $ do
      let out = renderPara [Link nullAttr [Str "https://example.com"] ("https://example.com", "")]
      out `shouldContainT` "link::https://example.com::"
      out `shouldNotContainT` "##"

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

    it "strips ./ prefix from internal link target" $ do
      let out = renderPara [Link nullAttr [Str "label"] ("./Classes/Foo", "")]
      out `shouldContainT` "link::Classes/Foo"
      out `shouldNotContainT` "./"

    it "internal link whose label matches the last path component omits ##" $
      renderPara [Link nullAttr [Str "Foo"] ("Classes/Foo", "")]
        `shouldNotContainT` "##"

    -- Disambiguation: an anchored link whose label contains '#'
    -- must use '##' as the separator, otherwise re-reading goes
    -- through the '#anchor#label' branch and 'splitLinkRecovering'
    -- (or just the greedy breakOnEnd) mis-attributes the trailing
    -- '#' fragment.
    it "anchored link with # in the label switches to ## separator" $ do
      let out = renderPara [Link nullAttr [Str "C#"] ("Foo#bar", "")]
      out `shouldContainT`    "link::Foo#bar##C#::"
      out `shouldNotContainT` "link::Foo#bar#C#::"

    it "link with empty URL renders ##label (no target)" $
      renderPara [Link nullAttr [Str "label"] ("", "")]
        `shouldBe` "link::##label::"

    it "link with empty label renders target only (no ##)" $
      renderPara [Link nullAttr [] ("https://example.com", "")]
        `shouldBe` "link::https://example.com::"


  -- -------------------------------------------------------------------------
  describe "method/argument inference" $ do

    it "H2 outside a method section is not inferred as method::" $ do
      let out = writeSCDocPure (doc [Header 2 nullAttr [Str "play"]])
      out `shouldContainT`    "subsection:: play"
      out `shouldNotContainT` "method::"

    it "H3 outside method context is not inferred as argument::" $ do
      let out = writeSCDocPure (doc [Header 3 nullAttr [Str "Notes"]])
      out `shouldContainT`    "subsubsection:: Notes"
      out `shouldNotContainT` "argument::"

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

    it "does not insert anchor:: for a heading whose ID is not referenced" $
      writeSCDocPure (doc [Header 2 ("unused-id", [], []) [Str "Unused"]])
        `shouldNotContainT` "anchor::"

    it "respects the no-anchor class" $ do
      let blocks =
            [ Header 2 ("sec", ["no-anchor"], []) [Str "Section"]
            , Para [Link nullAttr [Str "link"] ("#sec", "")]
            ]
      writeSCDocPure (doc blocks) `shouldNotContainT` "anchor::"


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
