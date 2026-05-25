{-# LANGUAGE OverloadedStrings #-}

module PropertySpec (spec) where

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Test.Hspec
import           Test.QuickCheck           (Arbitrary (..), Testable, choose,
                                            elements, frequency, isSuccess,
                                            quickCheckResult, sized, vectorOf,
                                            (===))

import           Text.Pandoc.Definition
import           Text.Pandoc.Readers.SCDoc (readSCDocPure)
import           Text.Pandoc.Writers.SCDoc

prop :: Testable p => String -> p -> Spec
prop name p = it name $ do
  result <- quickCheckResult p
  if isSuccess result
    then pure ()
    else expectationFailure ("QuickCheck falsified property: " ++ name)

spec :: Spec
spec = describe "properties" $ do

  describe "escapeInline" $ do
    prop "no bare :: remains after escaping" $ \s ->
      let escaped = escapeInline (T.pack s)
      in  not ("::" `T.isInfixOf` T.replace "\\::" "" escaped)

  describe "escapeBlockBody" $ do
    prop "no line is exactly :: after escaping" $ \s ->
      all (\l -> T.strip l /= "::") (T.lines (escapeBlockBody (T.pack s)))

    -- Backslash escapes are not idempotent; the bare-:: line case is.
    prop "single-line :: escape is stable" $
      escapeBlockBody (escapeBlockBody "::") === escapeBlockBody "::"

  describe "oneLine" $ do
    prop "idempotent" $ \s ->
      let t = T.pack s
      in  oneLine (oneLine t) == oneLine t

    prop "no consecutive spaces" $ \s ->
      not ("  " `T.isInfixOf` oneLine (T.pack s))

    prop "no leading or trailing whitespace" $ \s ->
      let t = oneLine (T.pack s)
      in  T.strip t == t

  describe "normalize" $ do
    prop "idempotent" $ \s ->
      let t = T.pack s
      in  normalize (normalize t) == normalize t

    prop "result is all lowercase" $ \s ->
      let t = normalize (T.pack s)
      in  T.toLower t == t

  describe "normalizeLinkTarget" $ do
    prop "no HelpSource/ prefix in result" $ \s ->
      not ("HelpSource/" `T.isPrefixOf` normalizeLinkTarget (T.pack s))

    prop "no ./ prefix in result" $ \s ->
      not ("./" `T.isPrefixOf` normalizeLinkTarget (T.pack s))

    prop "external URLs pass through unchanged" $ \s ->
      let t = T.pack s
      in  not (isExternalUrl t) || normalizeLinkTarget t == t

    prop "idempotent" $ \s ->
      let t = T.pack s
      in  normalizeLinkTarget (normalizeLinkTarget t) == normalizeLinkTarget t

  describe "isExternalUrl" $ do
    prop "case-insensitive" $ \s ->
      let t = T.pack s
      in  isExternalUrl t == isExternalUrl (T.toLower t)

  describe "linkHasAnchor" $ do
    prop "requires # to be present in target" $ \s ->
      let t = T.pack s
      in  not (linkHasAnchor t) || "#" `T.isInfixOf` t

    prop "is false when target contains ##" $ \s ->
      let t = T.pack s
      in  not (linkHasAnchor t) || not ("##" `T.isInfixOf` t)

    prop "is false for external URLs" $ \s ->
      let t = T.pack s
      in  not (linkHasAnchor t) || not (isExternalUrl t)

  -- Narrow modal generators focus on delimiter adjacency, not normalization.
  describe "inline modal round-trip" $ do

    prop "Code [verbatim t] round-trips through write/read" $
      \(VerbatimPayload t) ->
        roundTripInline (Code nullAttr t)
          === Right (Code nullAttr t)

    prop "Strong [Str t] round-trips through write/read" $
      \(TokenSafe t) ->
        roundTripInline (Strong [Str t])
          === Right (Strong [Str t])

    prop "Emph [Str t] round-trips through write/read" $
      \(TokenSafe t) ->
        roundTripInline (Emph [Str t])
          === Right (Emph [Str t])

    prop "Strikeout [Str t] round-trips through write/read" $
      \(TokenSafe t) ->
        roundTripInline (Strikeout [Str t])
          === Right (Strikeout [Str t])

    prop "Span teletype [Str t] round-trips through write/read" $
      \(TokenSafe t) ->
        roundTripInline (Span ("", ["teletype"], []) [Str t])
          === Right (Span ("", ["teletype"], []) [Str t])

    -- Deterministic pins for escaped delimiter payloads.
    it "Code with literal \\## round-trips" $
      roundTripInline (Code nullAttr "x\\##y")
        `shouldBe` Right (Code nullAttr "x\\##y")
    it "Code with literal \\|| round-trips" $
      roundTripInline (Code nullAttr "x\\||y")
        `shouldBe` Right (Code nullAttr "x\\||y")
    it "Code with literal \\:: round-trips" $
      roundTripInline (Code nullAttr "x\\::y")
        `shouldBe` Right (Code nullAttr "x\\::y")


-- | Pandoc-AST round-trip for a single inline.
roundTripInline :: Inline -> Either String Inline
roundTripInline i =
  let txt = writeSCDocPure (Pandoc nullMeta [Para [i]])
  in  case readSCDocPure txt of
        Left  err -> Left (show err)
        Right (Pandoc _ [Para [j]]) -> Right j
        Right pd  -> Left ("unexpected AST: " ++ show pd)


-- | Verbatim modal payload without leading/trailing space or newlines.
newtype VerbatimPayload = VerbatimPayload Text deriving Show

instance Arbitrary VerbatimPayload where
  arbitrary = sized $ \sz -> do
    n  <- choose (1, max 1 (min 20 (sz + 1)))
    cs <- vectorOf n verbatimChar
    -- Match verbatim whitespace normalization.
    let t = T.unwords (T.words (T.pack cs))
    pure (VerbatimPayload (if T.null t then "x" else t))
    where
      verbatimChar = frequency
        [ (8, elements alphaLower)
        , (4, pure ':')
        , (3, pure '\\')
        , (3, pure '#')
        , (3, pure '|')                     -- '\\||' is one of the lexer escapes
        , (2, pure ' ')                     -- internal spaces fine for verbatim
        , (2, elements alphaUpper)
        , (2, elements digits)
        , (1, elements puncSafe)
        ]


-- | Payload for modal tags whose body re-parses as inlines.
newtype TokenSafe = TokenSafe Text deriving Show

instance Arbitrary TokenSafe where
  arbitrary = sized $ \sz -> do
    n  <- choose (1, max 1 (min 20 (sz + 1)))
    cs <- vectorOf n tokenSafeChar
    let t = T.pack cs
    pure (TokenSafe (if T.null t then "x" else t))
    where
      tokenSafeChar = frequency
        [ (10, elements alphaLower)
        , (3,  pure '#')
        , (2,  pure '|')
        , (2,  elements alphaUpper)
        , (2,  elements digits)
        , (1,  elements puncSafe)
        ]


alphaLower, alphaUpper, digits, puncSafe :: String
alphaLower = ['a'..'z']
alphaUpper = ['A'..'Z']
digits     = ['0'..'9']
puncSafe   = "!?,.+-*/=()"
