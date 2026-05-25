{-# LANGUAGE OverloadedStrings #-}

-- | Opt-in full @HelpSource@ read/write/read audit.
-- Enabled by @SCDOC_AUDIT@; writes @/tmp/scdoc-audit.md@.
module CorpusAuditSpec (spec) where

import           Control.Monad              (forM)
import           Data.List                  (sort, sortBy)
import qualified Data.Map.Strict            as Map
import           Data.Ord                   (Down (..), comparing)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           System.Directory           (doesDirectoryExist,
                                             listDirectory)
import           System.Environment         (lookupEnv)
import           System.FilePath            (makeRelative, takeExtension,
                                             (</>))
import           Test.Hspec
import           Text.Read                  (readMaybe)

import           Text.Pandoc.Definition
import           Text.Pandoc.Readers.SCDoc  (readSCDocPure)
import           Text.Pandoc.Writers.SCDoc  (writeSCDocPure)


helpSourceEnv :: String
helpSourceEnv = "SCDOC_HELPSOURCE"


helpSourceRootFromEnv :: IO (Maybe FilePath)
helpSourceRootFromEnv = do
  root <- lookupEnv helpSourceEnv
  pure $ case root of
    Just path | not (null path) -> Just path
    _                           -> Nothing

reportPath :: FilePath
reportPath = "/tmp/scdoc-audit.md"


-- | Round-trip outcome for one file.
-- 'DriftAt' can carry the before/after blocks used in report samples.
data Drift
  = Identical
  | ParseFirst   String
  | ParseSecond  String
  | DriftAt Int  Text Text (Maybe Text) (Maybe (Block, Block))
  | MetaDrift Meta Meta
  deriving (Eq, Show)


-- | Grouping label for the report.
driftCategory :: Drift -> Text
driftCategory Identical               = "ok"
driftCategory (ParseFirst _)          = "parse-error (first read)"
driftCategory (ParseSecond _)         = "parse-error (re-read after write)"
driftCategory (DriftAt _ a b sub _)   =
  "drift: " <> a <> " -> " <> b <> maybe "" (\s -> " (" <> s <> ")") sub
driftCategory MetaDrift{}             = "drift: <meta> -> <meta>"


spec :: Spec
spec = do
  envOptIn     <- runIO (lookupEnv "SCDOC_AUDIT")
  mCorpusRoot  <- runIO helpSourceRootFromEnv
  corpusExists <- runIO $
    maybe (pure False) doesDirectoryExist mCorpusRoot
  case (envOptIn, mCorpusRoot, corpusExists) of
    (Nothing, _, _) -> pure ()
    (Just v, _, _) | null v -> pure ()
    (_, Nothing, _) ->
      describe "Opt-in HelpSource round-trip audit" $
        it ("requires " <> helpSourceEnv) $
          expectationFailure ("set " <> helpSourceEnv <> "=/path/to/HelpSource")
    (_, Just corpusRoot, False) ->
      describe "Opt-in HelpSource round-trip audit" $
        it "requires an existing HelpSource checkout" $
          expectationFailure ("corpus root not found at " <> corpusRoot)
    (_, Just corpusRoot, True) ->
      describe "Opt-in HelpSource round-trip audit" $ do
        files    <- runIO (sort <$> findSchelps corpusRoot)
        samples  <- runIO (samplesFromEnv defaultSamplesPerCategory)
        it ("audits " <> show (length files) <> " .schelp files") $ do
          results <- mapM auditFile files
          let summary = summarize results
              md      = formatReport corpusRoot samples (length files) summary results
          TIO.writeFile reportPath md
          putStrLn ""
          putStrLn ("Audit report written to " <> reportPath)
          mapM_ (putStrLn . T.unpack) (summaryLines summary)
          -- Informational only; the report drives follow-up slices.
          length files `shouldSatisfy` (>= 100)


-- | Default sample count per drift bucket.
defaultSamplesPerCategory :: Int
defaultSamplesPerCategory = 3


-- | Read @SCDOC_AUDIT_SAMPLES@, falling back on bad or missing input.
samplesFromEnv :: Int -> IO Int
samplesFromEnv fallback = do
  v <- lookupEnv "SCDOC_AUDIT_SAMPLES"
  pure $ case v >>= readMaybe of
    Just n | n > 0 -> n
    _              -> fallback


-- | Read each .schelp recursively, just like CorpusSpec.
findSchelps :: FilePath -> IO [FilePath]
findSchelps root = do
  entries <- listDirectory root
  fmap concat . forM entries $ \e -> do
    let p = root </> e
    isDir <- doesDirectoryExist p
    if isDir
      then findSchelps p
      else if takeExtension p == ".schelp"
        then pure [p]
        else pure []


-- | Round-trip one file: read, write, read again, compare.
auditFile :: FilePath -> IO (FilePath, Drift)
auditFile path = do
  input <- TIO.readFile path
  case readSCDocPure input of
    Left err -> pure (path, ParseFirst (show err))
    Right pd1 -> do
      let written = writeSCDocPure pd1
      case readSCDocPure written of
        Left err -> pure (path, ParseSecond (show err))
        Right pd2 ->
          if pd1 == pd2
            then pure (path, Identical)
            else pure (path, firstBlockDrift pd1 pd2)


-- | Find the first diverging top-level block, or report meta drift.
firstBlockDrift :: Pandoc -> Pandoc -> Drift
firstBlockDrift (Pandoc m1 bs1) (Pandoc m2 bs2)
  | m1 /= m2  = MetaDrift m1 m2
  | otherwise = go 0 bs1 bs2
  where
    go _ []     []     = DriftAt (-1) "<equal but flagged>" "<equal but flagged>" Nothing Nothing
    go i []     (b:_)  = DriftAt i "<missing>" (ctorOf b) Nothing Nothing
    go i (a:_)  []     = DriftAt i (ctorOf a) "<missing>" Nothing Nothing
    go i (a:as) (b:bs)
      | a == b    = go (i + 1) as bs
      | otherwise = DriftAt i (ctorOf a) (ctorOf b) (subClassify a b) (Just (a, b))


-- | Short identifier for the kind of block, used to group drift.
ctorOf :: Block -> Text
ctorOf b = case b of
  Plain{}          -> "Plain"
  Para{}           -> "Para"
  LineBlock{}      -> "LineBlock"
  CodeBlock{}      -> "CodeBlock"
  RawBlock{}       -> "RawBlock"
  BlockQuote{}     -> "BlockQuote"
  OrderedList{}    -> "OrderedList"
  BulletList{}     -> "BulletList"
  DefinitionList{} -> "DefinitionList"
  Header lvl _ _   -> "Header" <> T.pack (show lvl)
  HorizontalRule   -> "HorizontalRule"
  Table{}          -> "Table"
  Figure{}         -> "Figure"
  Div{}            -> "Div"


-- | Extra list-shape labels for list-to-list drift.
subClassify :: Block -> Block -> Maybe Text
subClassify (BulletList xs)    (BulletList _)    = Just (listSubcat xs)
subClassify (OrderedList _ xs) (OrderedList _ _) = Just (listSubcat xs)
subClassify _ _                                  = Nothing

-- | Sub-bucket label for list drift, based on source-side item shape.
listSubcat :: [[Block]] -> Text
listSubcat items =
  let shapes    = fmap itemShape items
      anyNested = HasNested `elem` shapes || Both `elem` shapes
      anyMulti  = HasMulti  `elem` shapes || Both `elem` shapes
  in case (anyNested, anyMulti) of
       (True,  True)  -> "mixed"
       (True,  False) -> "nested-list-flatten"
       (False, True)  -> "multi-block-item"
       (False, False) -> "other"

-- | Per-item shape used by 'listSubcat'.
data ItemShape = Simple | HasNested | HasMulti | Both
  deriving (Eq, Show)

-- | 'HasMulti' counts only non-list blocks; lead-in paragraphs are expected.
itemShape :: [Block] -> ItemShape
itemShape blocks =
  let nested      = any isList blocks
      nonListCnt  = length (filter (not . isList) blocks)
      multi       = nonListCnt > 1
  in case (nested, multi) of
       (True,  True)  -> Both
       (True,  False) -> HasNested
       (False, True)  -> HasMulti
       (False, False) -> Simple
  where
    isList BulletList{}    = True
    isList OrderedList{}   = True
    isList _               = False


-- | Category counts plus per-file drift details.
data Summary = Summary
  { sumTotal      :: Int
  , sumOk         :: Int
  , sumDrift      :: Int
  , sumByCategory :: Map.Map Text [(FilePath, Drift)]
  }

summarize :: [(FilePath, Drift)] -> Summary
summarize results =
  Summary
    { sumTotal      = length results
    , sumOk         = length [() | (_, Identical) <- results]
    , sumDrift      = length [() | (_, d) <- results, d /= Identical]
    , sumByCategory = Map.fromListWith (++)
        [ (driftCategory d, [(p, d)]) | (p, d) <- results, d /= Identical ]
    }

summaryLines :: Summary -> [Text]
summaryLines s =
  [ "Total:      " <> T.pack (show (sumTotal s))
  , "Identical:  " <> T.pack (show (sumOk s))
  , "Drifted:    " <> T.pack (show (sumDrift s))
  , ""
  , "Top drift categories:"
  ] <> fmap (\(cat, ps) -> "  " <> T.pack (show (length ps)) <> "  " <> cat)
            (take 10 (sortBy (comparing (Down . length . snd))
                             (Map.toList (sumByCategory s))))


-- | Markdown report with category lists and sample before/after blocks.
formatReport :: FilePath -> Int -> Int -> Summary -> [(FilePath, Drift)] -> Text
formatReport corpusRoot samples total s results = T.unlines $
  [ "# HelpSource round-trip audit"
  , ""
  , "Generated by `CorpusAuditSpec` against the corpus at"
  , "`" <> T.pack corpusRoot <> "`."
  , ""
  , "- Total files: " <> T.pack (show total)
  , "- Identical round-trip: " <> T.pack (show (sumOk s))
  , "- Drift (any non-equal AST): " <> T.pack (show (sumDrift s))
  , ""
  , "## Drift breakdown"
  , ""
  ] <> concatMap categorySection
        (sortBy (comparing (Down . length . snd)) (Map.toList (sumByCategory s)))
    <> [ "## Sample diffs"
       , ""
       , "First " <> T.pack (show samples)
         <> " files per bucket: before-block, after-block, and the"
       , "writer's output for the BEFORE block in isolation. The"
       , "isolation render may differ slightly from the in-context"
       , "writer output (no surrounding blank-line/escape interactions),"
       , "but is usually close enough to spot the bug. Set"
       , "`SCDOC_AUDIT_SAMPLES=N` to widen this section for triage."
       , ""
       ] <> concatMap categorySamples
            (sortBy (comparing (Down . length . snd)) (Map.toList (sumByCategory s)))
    <> [ "## Per-file detail (drift only)"
       , ""
       ]
    <> fmap lineFor [(p, d) | (p, d) <- results, d /= Identical]
  where
    rel = T.pack . makeRelative corpusRoot
    categorySection (cat, pds) =
      [ "### " <> cat <> " (" <> T.pack (show (length pds)) <> " files)"
      , ""
      ] <> fmap lineFor (take 20 (sortBy (comparing fst) pds))
        <> [ if length pds > 20
               then "- _... " <> T.pack (show (length pds - 20)) <> " more_"
               else ""
           , ""
           ]
    lineFor (p, d) =
      "- `" <> rel p <> "` -- " <> driftDetail d
    driftDetail Identical          = "ok"
    driftDetail (ParseFirst   e)   = "parse-error (first read): " <> truncErr (T.pack e)
    driftDetail (ParseSecond  e)   = "parse-error (re-read after write): " <> truncErr (T.pack e)
    driftDetail (DriftAt i a b sub _)
      | i < 0     = "<equal but flagged>"
      | otherwise =
          "drift at block " <> T.pack (show i) <> ": " <> a <> " -> " <> b
          <> maybe "" (\subtype -> " (" <> subtype <> ")") sub
    driftDetail MetaDrift{}        = "meta drift"
    -- Parsec errors include a multi-line message; keep just the
    -- first line so per-file lines stay scannable.
    truncErr msg =
      let trimmed = T.strip (T.takeWhile (/= '\n') msg)
      in if T.length trimmed > 120
           then T.take 117 trimmed <> "..."
           else trimmed

    categorySamples (cat, pds) =
      let chosen = take samples (sortBy (comparing fst) pds)
          blockPicks =
            [ (p, i, bBefore, bAfter)
            | (p, DriftAt i _ _ _ (Just (bBefore, bAfter))) <- chosen
            ]
          metaPicks =
            [ (p, m1, m2)
            | (p, MetaDrift m1 m2) <- chosen
            ]
          renderedBlocks = concatMap renderBlockSample blockPicks
          renderedMetas  = concatMap renderMetaSample  metaPicks
          body           = renderedBlocks <> renderedMetas
      in if null body
           then []
           else
             [ "### " <> cat
             , ""
             ] <> body

    renderBlockSample (p, i, bBefore, bAfter) =
      [ "#### `" <> rel p <> "` (block " <> T.pack (show i) <> ")"
      , ""
      , "Before:"
      , ""
      , "```"
      , T.pack (show bBefore)
      , "```"
      , ""
      , "After:"
      , ""
      , "```"
      , T.pack (show bAfter)
      , "```"
      , ""
      , "Writer output for BEFORE (in isolation):"
      , ""
      , "```"
      , T.stripEnd (renderBlockInIsolation bBefore)
      , "```"
      , ""
      ]

    renderMetaSample (p, m1, m2) =
      [ "#### `" <> rel p <> "` (meta)"
      , ""
      , "Before:"
      , ""
      , "```"
      , T.pack (show m1)
      , "```"
      , ""
      , "After:"
      , ""
      , "```"
      , T.pack (show m2)
      , "```"
      , ""
      ]


-- | Render one block through the writer for report samples.
renderBlockInIsolation :: Block -> Text
renderBlockInIsolation b = writeSCDocPure (Pandoc nullMeta [b])
