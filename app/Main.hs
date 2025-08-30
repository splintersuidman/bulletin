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
import           System.Environment      (getArgs)
import           System.Exit             (die)
import           System.FilePath         (takeExtension)
import           Text.Pandoc
import qualified Text.Pandoc.Builder     as P
import           Text.Pandoc.Builder     (Blocks, ToMetaValue (toMetaValue))
import qualified Text.Pandoc.PDF         as Pandoc
import qualified Text.Pandoc.Transforms  as Pandoc
import qualified Text.Pandoc.UTF8        as Pandoc
import           Text.Pandoc.Walk        (query)
import qualified Toml
import           Toml                    (TomlCodec, TomlDecodeError, (.=))

data Bulletin doc = Bulletin
  { bulletinTitle         :: !Text
  , bulletinDate          :: !Time.Day
  , bulletinFilename      :: !FilePath
  , bulletinTemplate      :: !FilePath
  , bulletinExtra         :: !(Map Text Text)
  , bulletinContributions :: [Contribution doc]
  } deriving (Show)

mapBulletin :: (Contribution a -> Contribution b) -> Bulletin a -> Bulletin b
mapBulletin f bulletin = bulletin
  { bulletinContributions = fmap f (bulletinContributions bulletin) }

mapBulletinM :: Monad m => (Contribution a -> m (Contribution b)) -> Bulletin a -> m (Bulletin b)
mapBulletinM f bulletin = do
  contributions <- mapM f $ bulletinContributions bulletin
  pure $ bulletin { bulletinContributions = contributions }

bulletinCodec :: TomlCodec (Bulletin Source)
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
  = BulletinUnsupportedFormat String
  | BulletinPandocError PandocError
  | BulletinTomlDecodeError [TomlDecodeError]
  | BulletinCompileError BL.ByteString
  | BulletinTemplateError String
  | BulletinUsageError String

instance Show BulletinError where
  show = \case
    BulletinUnsupportedFormat fmt -> "Unsupported file format for contribution: " <> fmt
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

liftPandocIO :: PandocIO a -> BulletinIO a
liftPandocIO mx = do
  res <- liftIO $ runIO mx
  liftEither' BulletinPandocError res

sourceExtension :: Source -> String
sourceExtension = \case
  SourceFile filename -> takeExtension filename
  SourceUrl url -> takeExtension $ Text.unpack $ url
  SourceGoogleDocs _ -> ".docx"

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

readPandocLazy :: Contribution Source
               -> (forall m. PandocMonad m => ReaderOptions -> BL.ByteString -> m Pandoc)
               -> BulletinIO (Contribution Pandoc)
readPandocLazy contribution reader = do
  let source = contributionDocument contribution
  content <- readSourceLazy source
  doc <- liftPandocIO $ reader def content
  pure $ doc <$ contribution

readPandoc :: Contribution Source
           -> (forall m. PandocMonad m => ReaderOptions -> Text -> m Pandoc)
           -> BulletinIO (Contribution Pandoc)
readPandoc contribution reader = do
  let source = contributionDocument contribution
  content <- readSource source
  doc <- liftPandocIO $ reader def content
  pure $ doc <$ contribution

-- | Read a contribution and parse it into Pandoc's AST.
readContribution :: Contribution Source -> BulletinIO (Contribution Pandoc)
readContribution contribution = do
  let extension = sourceExtension $ contributionDocument contribution
  case extension  of
    ".docx" -> readPandocLazy contribution readDocx
    ".md"   -> readPandoc contribution readMarkdown
    ".odt"  -> readPandocLazy contribution readODT
    ".rtf"  -> readPandoc contribution readRTF
    ".typ"  -> readPandoc contribution readTypst
    _       -> throwError $ BulletinUnsupportedFormat extension

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
readBulletinConfig :: FilePath -> BulletinIO (Bulletin Source)
readBulletinConfig filename = do
  res <- Toml.decodeFileEither bulletinCodec filename
  liftEither' BulletinTomlDecodeError res

-- | Read the contributions specified in the given bulletin
-- configuration.
readContributions :: Bulletin Source -> BulletinIO (Bulletin Pandoc)
readContributions = mapBulletinM readContribution

-- | Process the contributions.
processContributions :: Bulletin Pandoc -> Bulletin Blocks
processContributions = mapBulletin processContribution

instance ToMetaValue a => ToMetaValue (Contribution a) where
  toMetaValue contribution = toMetaValue @(Map Text MetaValue) $ Map.fromList
    [ ("title", toMetaValue $ contributionTitle contribution)
    , ("author", toMetaValue $ contributionAuthor contribution)
    , ("date", toMetaValue $ Time.showGregorian $ contributionDate contribution)
    , ("body", toMetaValue $ contributionDocument contribution)
    ]

-- | Compile the bulletin, concatenating the contributions together
-- into a single document and setting the required variables.
compileBulletin :: Bulletin Blocks -> Pandoc
compileBulletin bulletin
  = P.setTitle (P.text $ bulletinTitle bulletin)
  $ P.setDate (P.text $ Text.pack $ Time.showGregorian $ bulletinDate bulletin)
  $ P.setMeta "extra" (bulletinExtra bulletin)
  $ P.setMeta "contributions" (toMetaValue $ bulletinContributions bulletin)
  $ mempty

-- | Read and compile the template file specified in the bulletin.
readTemplate :: Bulletin doc -> BulletinIO (Template Text)
readTemplate bulletin = do
  templateText <- liftIO $ Text.readFile (bulletinTemplate bulletin)
  templateRes <- liftIO $ compileTemplate (bulletinTemplate bulletin) templateText
  liftEither' BulletinTemplateError templateRes

-- | Write the bulletin as a PDF file, compiling it with Typst.
writeBulletin :: Bulletin doc -> Template Text -> Pandoc -> BulletinIO ()
writeBulletin bulletin template doc = do
  compileRes <- liftPandocIO $
    Pandoc.makePDF
      "typst"
      []
      writeTypst
      def { writerTemplate = Just template }
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
    bulletin <- processContributions <$> readContributions bulletinConfig
    template <- readTemplate bulletin
    writeBulletin bulletin template $ compileBulletin bulletin
