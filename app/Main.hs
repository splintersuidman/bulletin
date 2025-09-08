{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative     ((<|>))
import           Control.Lens            ((^.))
import           Control.Monad           (when)
import           Control.Monad.Except    (ExceptT, MonadError (throwError),
                                          runExceptT)
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import qualified Data.ByteString.Lazy    as BL
import           Data.Foldable           (for_)
import           Data.List               (intercalate)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (fromMaybe)
import           Data.Semigroup          (Min (Min, getMin))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time               as Time
import qualified Network.Wreq            as Wreq
import           System.Directory        (setCurrentDirectory)
import           System.Environment      (getArgs)
import           System.Exit             (die)
import           System.FilePath         (takeBaseName, takeDirectory)
import           Text.Pandoc
import qualified Text.Pandoc.Builder     as P
import           Text.Pandoc.Builder     (Blocks, ToMetaValue (toMetaValue))
import qualified Text.Pandoc.Extensions  as Pandoc
import qualified Text.Pandoc.Format      as Pandoc
import qualified Text.Pandoc.PDF         as Pandoc
import qualified Text.Pandoc.Readers     as Pandoc
import qualified Text.Pandoc.Transforms  as Pandoc
import qualified Text.Pandoc.UTF8        as Pandoc
import           Text.Pandoc.Walk        (query)
import qualified Text.Pandoc.Writers     as Pandoc
import qualified Toml
import           Toml                    (TomlCodec, TomlDecodeError, (.=))

data Bulletin templ doc = Bulletin
  { bulletinTitle         :: !Text
  , bulletinDate          :: !Time.Day
  , bulletinExtra         :: !(Map Text Text)
  , bulletinOutputs       :: ![Output templ]
  , bulletinContributions :: [Contribution doc]
  } deriving (Show)

mapBulletin :: (Output t -> Output t') -> (Contribution d -> Contribution d') -> Bulletin t d -> Bulletin t' d'
mapBulletin f g bulletin = bulletin
  { bulletinOutputs = fmap f (bulletinOutputs bulletin)
  , bulletinContributions = fmap g (bulletinContributions bulletin)
  }

mapBulletinM :: Monad m
             => (Output t -> m (Output t'))
             -> (Contribution d -> m (Contribution d'))
             -> Bulletin t d
             -> m (Bulletin t' d')
mapBulletinM f g bulletin = do
  outputs <- mapM f $ bulletinOutputs bulletin
  contributions <- mapM g $ bulletinContributions bulletin
  pure $ bulletin
    { bulletinOutputs = outputs
    , bulletinContributions = contributions
    }

bulletinCodec :: TomlCodec (Bulletin FilePath Input)
bulletinCodec = Bulletin
  <$> Toml.text                             "title"        .= bulletinTitle
  <*> Toml.day                              "date"         .= bulletinDate
  <*> Toml.tableMap Toml._KeyText Toml.text "extra"        .= bulletinExtra
  <*> Toml.list outputCodec                 "output"       .= bulletinOutputs
  <*> Toml.list contributionCodec           "contribution" .= bulletinContributions

data Output templ = Output
  { outputFilename :: !FilePath
  , outputFormat   :: !OutputFormat
  , outputTemplate :: templ
  } deriving (Show, Functor)

outputCodec :: TomlCodec (Output FilePath)
outputCodec = Output
  <$> Toml.string "file"     .= outputFilename
  <*> outputFormatCodec      .= outputFormat
  <*> Toml.string "template" .= outputTemplate

data OutputFormat
  = OutputFormat Text
    -- ^ Output format for all formats except PDF (supported by writers in 'Pandoc.writers')
  | OutputFormatPdf Text
    -- ^ PDF output with specified compiler (supported by 'Pandoc.makePDF')
  | OutputFormatUnspecified
  deriving (Show)

matchOutputFormat :: OutputFormat -> Maybe Text
matchOutputFormat = \case
  OutputFormat format -> Just format
  _ -> Nothing

matchOutputFormatPdf :: OutputFormat -> Maybe Text
matchOutputFormatPdf = \case
  OutputFormatPdf compiler -> Just compiler
  _ -> Nothing

matchOutputFormatUnspecified :: OutputFormat -> Maybe ()
matchOutputFormatUnspecified = \case
  OutputFormatUnspecified -> Just ()
  _ -> Nothing

outputFormatCodec :: TomlCodec OutputFormat
outputFormatCodec
   =  Toml.dimatch matchOutputFormatPdf OutputFormatPdf pdf
  <|> Toml.dimatch matchOutputFormat OutputFormat otherFormat
  <|> pure OutputFormatUnspecified
  where
    pdf = Toml.hardcoded "pdf" Toml._Text "format" *> Toml.text "compiler"
    validateOtherFormat format = if format == "pdf"
      then Left "PDF output format should be accompanied by compiler option"
      else Right format
    otherFormat = Toml.validate validateOtherFormat Toml._Text "format"

data Contribution doc = Contribution
  { contributionAuthor   :: !Text
  , contributionTitle    :: !Text
  , contributionDate     :: !Time.Day
  , contributionDocument :: doc
  } deriving (Show, Functor)

mapContribution' :: (Contribution a -> b) -> Contribution a -> Contribution b
mapContribution' f contribution = contribution
  { contributionDocument = f contribution }

contributionCodec :: TomlCodec (Contribution Input)
contributionCodec = Contribution
  <$> Toml.text   "author" .= contributionAuthor
  <*> Toml.text   "title"  .= contributionTitle
  <*> Toml.day    "date"   .= contributionDate
  <*> inputCodec           .= contributionDocument

data Input = Input
  { inputSource :: !Source
  , inputFormat :: !(Maybe Text)
  } deriving (Show)

inputCodec :: TomlCodec Input
inputCodec = Input
  <$> sourceCodec                          .= inputSource
  <*> Toml.dioptional (Toml.text "format") .= inputFormat

data Source
  = SourceFile !FilePath
  | SourceUrl !Text
  | SourceGoogleDocs !Text
  deriving (Show)

matchSourceFile :: Source -> Maybe FilePath
matchSourceFile = \case
  SourceFile file -> Just file
  _ -> Nothing

matchSourceUrl :: Source -> Maybe Text
matchSourceUrl = \case
  SourceUrl url -> Just url
  _ -> Nothing

matchSourceGoogleDocs :: Source -> Maybe Text
matchSourceGoogleDocs = \case
  SourceGoogleDocs docId -> Just docId
  _ -> Nothing

sourceCodec :: TomlCodec Source
sourceCodec
   =  Toml.dimatch matchSourceFile SourceFile (Toml.string "file")
  <|> Toml.dimatch matchSourceUrl SourceUrl (Toml.text "url")
  <|> Toml.dimatch matchSourceGoogleDocs SourceGoogleDocs (Toml.text "google-docs")

data BulletinError
  = BulletinUnsupportedInputFormat Input
  | BulletinUnsupportedOutputFormat String
  | BulletinPandocError PandocError
  | BulletinTomlDecodeError [TomlDecodeError]
  | BulletinCompileError BL.ByteString
  | BulletinTemplateError String
  | BulletinUsageError String
  | BulletinUnsupportedPdfCompiler Text

instance Show BulletinError where
  show = \case
    BulletinUnsupportedInputFormat fmt -> "Unsupported file format for contribution: " <> show fmt
    BulletinUnsupportedOutputFormat fmt -> "Unsupported file format for output: " <> show fmt
    BulletinPandocError err -> "Pandoc error: " <> show err
    BulletinTomlDecodeError errs -> "Toml error(s): " <> intercalate "\n" (fmap show errs)
    BulletinCompileError err -> "Compile error: " <> Pandoc.toStringLazy err
    BulletinTemplateError err -> "Template error: " <> err
    BulletinUsageError err -> "Usage error: " <> err
    BulletinUnsupportedPdfCompiler compiler -> "Unsupported PDF compiler: " <> Text.unpack compiler

newtype BulletinIO a = BulletinIO { unBulletinIO :: ExceptT BulletinError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError BulletinError)

runBulletinIO :: BulletinIO a -> IO a
runBulletinIO mx = do
  res <- runExceptT $ unBulletinIO mx
  either (die . show) pure res

liftEither' :: MonadError e m => (e' -> e) -> Either e' a -> m a
liftEither' f = either (throwError . f) pure

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe err = maybe (throwError err) pure

liftPandocIO :: PandocIO a -> BulletinIO a
liftPandocIO mx = do
  res <- liftIO $ runIO mx
  liftEither' BulletinPandocError res

defaultGoogleDocsFormat :: Text
defaultGoogleDocsFormat  = "docx"

formatFromSource :: Source -> Maybe Pandoc.FlavoredFormat
formatFromSource = \case
  SourceFile filename -> Pandoc.formatFromFilePaths [filename]
  SourceUrl url -> Pandoc.formatFromFilePaths [Text.unpack url]
  SourceGoogleDocs _ -> Just $ Pandoc.FlavoredFormat
    { formatName = defaultGoogleDocsFormat
    , formatExtsDiff = mempty
    }

formatFromInput :: Input -> BulletinIO Pandoc.FlavoredFormat
formatFromInput input = case inputFormat input of
  Nothing     -> liftMaybe (BulletinUnsupportedInputFormat input) $ formatFromSource $ inputSource input
  Just format -> liftPandocIO $ Pandoc.parseFlavoredFormat format

readUrl :: Text -> BulletinIO BL.ByteString
readUrl url = liftIO $ (^. Wreq.responseBody) <$> Wreq.get (Text.unpack url)

readGoogleDocs :: Text -> Maybe Text -> BulletinIO BL.ByteString
readGoogleDocs docId format = do
  readUrl $ "https://docs.google.com/document/d/" <> docId <> "/export?format=" <> fromMaybe defaultGoogleDocsFormat format

readInputLazy :: Input -> BulletinIO BL.ByteString
readInputLazy input = case inputSource input of
  SourceFile filename    -> liftPandocIO $ readFileLazy filename
  SourceUrl url          -> readUrl url
  SourceGoogleDocs docId -> readGoogleDocs docId (inputFormat input)

readInput :: Input -> BulletinIO Text
readInput = fmap (TL.toStrict . TL.decodeUtf8) . readInputLazy

readPandoc :: Pandoc.Reader PandocIO -> Pandoc.Extensions -> Contribution Input -> BulletinIO (Contribution Pandoc)
readPandoc reader extensions contribution = do
  let input = contributionDocument contribution
  let readerOptions = def { readerExtensions = extensions }
  doc <- case reader of
    Pandoc.ByteStringReader r -> liftPandocIO . r readerOptions =<< readInputLazy input
    Pandoc.TextReader r -> liftPandocIO . r readerOptions =<< readInput input
  pure $ doc <$ contribution

-- | Read a contribution and parse it into Pandoc"s AST.
readContribution :: Contribution Input -> BulletinIO (Contribution Pandoc)
readContribution contribution = do
  let input = contributionDocument contribution
  format <- formatFromInput input
  (reader, extensions) <- liftPandocIO $ Pandoc.getReader format
  readPandoc reader extensions contribution

-- | Obtain the minimal header level from the document, if any headers
-- are present.
minHeaderLevel :: Pandoc -> Maybe Int
minHeaderLevel = fmap getMin . query minHeader
  where
    minHeader :: Block -> Maybe (Min Int)
    minHeader (Header level _ _) = Just $ Min level
    minHeader _                  = Nothing

-- | Possibly correct the header levels, if top-level headers are used.
correctHeaderLevels :: Pandoc -> Pandoc
correctHeaderLevels p = case minHeaderLevel p of
  Just l | l == 1 -> Pandoc.headerShift 1 p
  _               -> p

-- | Add header to a contribution, consisting of a pagebreak, a header
-- with the title, and the author set in italic.
addContributionHeader :: Contribution Pandoc -> Pandoc
addContributionHeader contribution = case contributionDocument contribution of
  Pandoc meta blocks -> Pandoc meta $ [Header 1 nullAttr title, Para author] <> blocks
  where
    title = P.toList $ P.text $ contributionTitle contribution
    author = P.toList $ P.emph $ P.text $ contributionAuthor contribution

-- | Extract the blocks from a contribution document, discarding the
-- meta values.
extractBlocks :: Contribution Pandoc -> Blocks
extractBlocks contribution = case contributionDocument contribution of
  Pandoc _meta blocks -> P.fromList blocks

-- | Perform all necessary transformations to make an input
-- contribution ready to be used as part of output.
processContribution :: Contribution Pandoc -> Contribution Blocks
processContribution = mapContribution' extractBlocks . fmap correctHeaderLevels

-- | Read the bulletin configuration from the given file.
readBulletinConfig :: FilePath -> BulletinIO (Bulletin FilePath Input)
readBulletinConfig filename = do
  res <- Toml.decodeFileEither bulletinCodec filename
  liftEither' BulletinTomlDecodeError res

-- | Read the contributions specified in the given bulletin
-- configuration.
readContributions :: Bulletin templ Input -> BulletinIO (Bulletin templ Pandoc)
readContributions = mapBulletinM pure readContribution

-- | Process the contributions.
processContributions :: Bulletin templ Pandoc -> Bulletin templ Blocks
processContributions = mapBulletin id processContribution

instance ToMetaValue a => ToMetaValue (Contribution a) where
  toMetaValue contribution = toMetaValue @(Map Text MetaValue) $ Map.fromList
    [ ("title", toMetaValue $ contributionTitle contribution)
    , ("author", toMetaValue $ contributionAuthor contribution)
    , ("date", toMetaValue $ Time.showGregorian $ contributionDate contribution)
    , ("body", toMetaValue $ contributionDocument contribution)
    ]

-- | Compile the bulletin, concatenating the contributions together
-- into a single document and setting the required variables.
compileBulletin :: Bulletin templ Blocks -> Pandoc
compileBulletin bulletin
  = P.setTitle (P.text $ bulletinTitle bulletin)
  $ P.setDate (P.text $ Text.pack $ Time.showGregorian $ bulletinDate bulletin)
  $ P.setMeta "extra" (bulletinExtra bulletin)
  $ P.setMeta "contributions" (toMetaValue $ bulletinContributions bulletin)
  $ mempty

-- | Read and compile the template file specified in the output.
readTemplate :: Output FilePath -> BulletinIO (Output (Template Text))
readTemplate output = do
  let templateFile = outputTemplate output
  templateText <- liftIO $ Text.readFile templateFile
  templateRes <- liftIO $ compileTemplate templateFile templateText
  template <- liftEither' BulletinTemplateError templateRes
  pure $ template <$ output

-- | Read and compile the template files specified in the outputs of
-- the bulletin.
readTemplates :: Bulletin FilePath doc -> BulletinIO (Bulletin (Template Text) doc)
readTemplates = mapBulletinM readTemplate pure

formatFromOutput :: FilePath -> Maybe Text -> BulletinIO Pandoc.FlavoredFormat
formatFromOutput filename = \case
  Nothing -> liftMaybe (BulletinUnsupportedOutputFormat $ filename) $
    Pandoc.formatFromFilePaths [filename]
  Just format -> liftPandocIO $ Pandoc.parseFlavoredFormat format

writeOutputNormal :: Pandoc -> Output (Template Text) -> Maybe Text -> BulletinIO ()
writeOutputNormal doc output fmt = do
  format <- formatFromOutput (outputFilename output) fmt
  (writer, extensions) <- liftPandocIO $ Pandoc.getWriter format
  let writerOptions = def
        { writerExtensions = extensions
        , writerTemplate = Just (outputTemplate output)
        }
  case writer of
    Pandoc.ByteStringWriter w -> liftIO . BL.writeFile (outputFilename output) =<< liftPandocIO (w writerOptions doc)
    Pandoc.TextWriter w       -> liftIO . Text.writeFile (outputFilename output) =<< liftPandocIO (w writerOptions doc)

compilerToWriter :: Text -> BulletinIO (WriterOptions -> Pandoc -> PandocIO Text)
compilerToWriter compiler = case takeBaseName (Text.unpack compiler) of
  "wkhtmltopdf"  -> pure writeHtml5String
  "pagedjs-cli"  -> pure writeHtml5String
  "prince"       -> pure writeHtml5String
  "weasyprint"   -> pure writeHtml5String
  "typst"        -> pure writeTypst
  "pdfroff"      -> pure writeMs
  "groff"        -> pure writeMs
  "context"      -> pure writeLaTeX
  "tectonic"     -> pure writeLaTeX
  "latexmk"      -> pure writeLaTeX
  "lualatex"     -> pure writeLaTeX
  "lualatex-dev" -> pure writeLaTeX
  "pdflatex"     -> pure writeLaTeX
  "pdflatex-dev" -> pure writeLaTeX
  "xelatex"      -> pure writeLaTeX
  _              -> throwError $ BulletinUnsupportedPdfCompiler compiler

writeOutputPdf :: Pandoc -> Output (Template Text) -> Text -> BulletinIO ()
writeOutputPdf doc output compiler = do
  writer <- compilerToWriter compiler
  let writerOptions = def { writerTemplate = Just (outputTemplate output) }
  compileRes <- liftPandocIO $ Pandoc.makePDF (Text.unpack compiler) [] writer writerOptions doc
  out <- liftEither' BulletinCompileError compileRes
  liftIO $ BL.writeFile (outputFilename output) out

-- | Write the bulletin as a PDF file, compiling it with Typst.
writeOutput :: Pandoc -> Output (Template Text) -> BulletinIO ()
writeOutput doc output = case outputFormat output of
  OutputFormatUnspecified  -> writeOutputNormal doc output Nothing
  OutputFormat format      -> writeOutputNormal doc output (Just format)
  OutputFormatPdf compiler -> writeOutputPdf doc output compiler

writeOutputs :: Bulletin (Template Text) doc -> Pandoc -> BulletinIO ()
writeOutputs bulletin doc = for_ (bulletinOutputs bulletin) $ writeOutput doc

main :: IO ()
main = runBulletinIO $ do
  args <- liftIO getArgs
  when (null args) $
    throwError $ BulletinUsageError "Specify configuration file(s): bulletin <file ...>"

  for_ args $ \configFile -> do
    bulletinConfig <- readBulletinConfig configFile
    -- Set directory to that of config file, to deal with paths relative to it.
    liftIO $ setCurrentDirectory $ takeDirectory configFile
    bulletin <- readTemplates . processContributions =<< readContributions bulletinConfig
    writeOutputs bulletin $ compileBulletin bulletin
