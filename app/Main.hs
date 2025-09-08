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
import           System.FilePath         (takeDirectory, takeExtension)
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
import qualified Toml
import           Toml                    (TomlCodec, TomlDecodeError, (.=))

data Bulletin templ doc = Bulletin
  { bulletinTitle         :: !Text
  , bulletinDate          :: !Time.Day
  , bulletinFilename      :: !FilePath
  , bulletinTemplate      :: !templ
  , bulletinExtra         :: !(Map Text Text)
  , bulletinContributions :: [Contribution doc]
  } deriving (Show)

mapBulletin :: (t -> t') -> (Contribution d -> Contribution d') -> Bulletin t d -> Bulletin t' d'
mapBulletin f g bulletin = bulletin
  { bulletinTemplate = f (bulletinTemplate bulletin)
  , bulletinContributions = fmap g (bulletinContributions bulletin)
  }

mapBulletinM :: Monad m => (t -> m t') -> (Contribution d -> m (Contribution d')) -> Bulletin t d -> m (Bulletin t' d')
mapBulletinM f g bulletin = do
  template <- f $ bulletinTemplate bulletin
  contributions <- mapM g $ bulletinContributions bulletin
  pure $ bulletin
    { bulletinTemplate = template
    , bulletinContributions = contributions
    }

bulletinCodec :: TomlCodec (Bulletin FilePath Source)
bulletinCodec = Bulletin
  <$> Toml.text                             "title"        .= bulletinTitle
  <*> Toml.day                              "date"         .= bulletinDate
  <*> Toml.string                           "file"         .= bulletinFilename
  <*> Toml.string                           "template"     .= bulletinTemplate
  <*> Toml.tableMap Toml._KeyText Toml.text "extra"        .= bulletinExtra
  <*> Toml.list contributionCodec           "contribution" .= bulletinContributions

data Contribution doc = Contribution
  { contributionAuthor   :: !Text
  , contributionTitle    :: !Text
  , contributionDate     :: !Time.Day
  , contributionDocument :: doc
  } deriving (Show, Functor)

mapContribution' :: (Contribution a -> b) -> Contribution a -> Contribution b
mapContribution' f contribution = contribution
  { contributionDocument = f contribution }

mapContributionM :: Monad m => (a -> m b) -> Contribution a -> m (Contribution b)
mapContributionM f contribution = do
  doc <- f (contributionDocument contribution)
  pure $ fmap (const doc) contribution

contributionCodec :: TomlCodec (Contribution Source)
contributionCodec = Contribution
  <$> Toml.text   "author" .= contributionAuthor
  <*> Toml.text   "title"  .= contributionTitle
  <*> Toml.day    "date"   .= contributionDate
  <*> sourceCodec          .= contributionDocument

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
  = BulletinUnsupportedFormat Source
  | BulletinPandocError PandocError
  | BulletinTomlDecodeError [TomlDecodeError]
  | BulletinCompileError BL.ByteString
  | BulletinTemplateError String
  | BulletinUsageError String

instance Show BulletinError where
  show = \case
    BulletinUnsupportedFormat fmt -> "Unsupported file format for contribution: " <> show fmt
    BulletinPandocError err -> "Pandoc error: " <> show err
    BulletinTomlDecodeError errs -> "Toml error(s): " <> intercalate "\n" (fmap show errs)
    BulletinCompileError err -> "Compile error: " <> Pandoc.toStringLazy err
    BulletinTemplateError err -> "Template error: " <> err
    BulletinUsageError err -> "Usage error: " <> err

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

sourceExtension :: Source -> String
sourceExtension = \case
  SourceFile filename -> takeExtension filename
  SourceUrl url -> takeExtension $ Text.unpack url
  SourceGoogleDocs _ -> ".docx"

formatFromSource :: Source -> Maybe Pandoc.FlavoredFormat
formatFromSource = \case
  SourceFile filename -> Pandoc.formatFromFilePaths [filename]
  SourceUrl url -> Pandoc.formatFromFilePaths [Text.unpack url]
  SourceGoogleDocs _ -> Just $ Pandoc.FlavoredFormat { formatName = "docx", formatExtsDiff = mempty }

readUrl :: Text -> BulletinIO BL.ByteString
readUrl url = liftIO $ (^. Wreq.responseBody) <$> Wreq.get (Text.unpack url)

readGoogleDocs :: Text -> BulletinIO BL.ByteString
readGoogleDocs docId = readUrl $ "https://docs.google.com/document/d/" <> docId <> "/export?format=docx"

readSourceLazy :: Source -> BulletinIO BL.ByteString
readSourceLazy = \case
  SourceFile filename -> liftPandocIO $ readFileLazy filename
  SourceUrl url -> readUrl url
  SourceGoogleDocs docId -> readGoogleDocs docId

readSource :: Source -> BulletinIO Text
readSource = fmap (TL.toStrict . TL.decodeUtf8) . readSourceLazy

readPandoc :: Pandoc.Reader PandocIO -> Pandoc.Extensions -> Contribution Source -> BulletinIO (Contribution Pandoc)
readPandoc reader extensions contribution = do
  let source = contributionDocument contribution
  let readerOptions = def { readerExtensions = extensions }
  doc <- case reader of
    Pandoc.ByteStringReader r -> liftPandocIO . r readerOptions =<< readSourceLazy source
    Pandoc.TextReader r -> liftPandocIO . r readerOptions =<< readSource source
  pure $ doc <$ contribution

-- | Read a contribution and parse it into Pandoc's AST.
readContribution :: Contribution Source -> BulletinIO (Contribution Pandoc)
readContribution contribution = do
  let source = contributionDocument contribution
  format <- liftMaybe (BulletinUnsupportedFormat source) $ formatFromSource source
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
readBulletinConfig :: FilePath -> BulletinIO (Bulletin FilePath Source)
readBulletinConfig filename = do
  res <- Toml.decodeFileEither bulletinCodec filename
  liftEither' BulletinTomlDecodeError res

-- | Read the contributions specified in the given bulletin
-- configuration.
readContributions :: Bulletin FilePath Source -> BulletinIO (Bulletin FilePath Pandoc)
readContributions = mapBulletinM pure readContribution

-- | Process the contributions.
processContributions :: Bulletin FilePath Pandoc -> Bulletin FilePath Blocks
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

-- | Read and compile the template file specified in the bulletin.
readTemplate :: Bulletin FilePath doc -> BulletinIO (Bulletin (Template Text) doc)
readTemplate bulletin = do
  templateText <- liftIO $ Text.readFile (bulletinTemplate bulletin)
  templateRes <- liftIO $ compileTemplate (bulletinTemplate bulletin) templateText
  template <- liftEither' BulletinTemplateError templateRes
  pure $ mapBulletin (const template) id bulletin

-- | Write the bulletin as a PDF file, compiling it with Typst.
writeBulletin :: Bulletin (Template Text) doc -> Pandoc -> BulletinIO ()
writeBulletin bulletin doc = do
  compileRes <- liftPandocIO $
    Pandoc.makePDF
      "typst"
      []
      writeTypst
      def { writerTemplate = Just (bulletinTemplate bulletin) }
      doc
  out <- liftEither' BulletinCompileError compileRes
  liftIO $ BL.writeFile (bulletinFilename bulletin) out

main :: IO ()
main = runBulletinIO $ do
  args <- liftIO getArgs
  when (null args) $
    throwError $ BulletinUsageError "Specify configuration file(s): bulletin <file ...>"

  for_ args $ \configFile -> do
    bulletinConfig <- readBulletinConfig configFile
    -- Set directory to that of config file, to deal with paths relative to it.
    liftIO $ setCurrentDirectory $ takeDirectory configFile
    bulletin <- readTemplate . processContributions =<< readContributions bulletinConfig
    writeBulletin bulletin $ compileBulletin bulletin
