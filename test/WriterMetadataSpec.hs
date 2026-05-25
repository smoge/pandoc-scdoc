{-# LANGUAGE OverloadedStrings #-}

module WriterMetadataSpec (spec) where

import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Test.Hspec

import qualified Text.DocTemplates          as DT
import qualified Text.Pandoc                as Pandoc
import           Text.Pandoc.Definition
import           Text.Pandoc.Options        (WriterOptions (..), def)
import           Text.Pandoc.Writers.SCDoc
import           Text.Pandoc.Writers.Shared (defField)

import           TestSupport

spec :: Spec
spec = do
  describe "scHeader" $ do

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


  describe "keyword injection" $ do

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
