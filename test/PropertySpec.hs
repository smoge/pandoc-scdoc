{-# LANGUAGE OverloadedStrings #-}

module PropertySpec (spec) where

import qualified Data.Text                 as T
import           Test.Hspec
import           Test.QuickCheck           (Testable, isSuccess,
                                            quickCheckResult)

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

    prop "idempotent" $ \s ->
      let t = T.pack s
      in  escapeBlockBody (escapeBlockBody t) == escapeBlockBody t

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
