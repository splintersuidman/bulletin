{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (when)
import           Control.Monad.Except   (ExceptT, MonadError (throwError),
                                         runExceptT)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy   as BL
import           Data.List              (intersperse)
import           Data.Map.Strict        (Map)
import           Data.Semigroup         (Min (Min, getMin))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import qualified Data.Time              as Time
import           Data.Traversable       (for)
import           System.Environment     (getArgs)
import           System.Exit            (die)
import           System.FilePath        (takeExtension)
import           Text.Pandoc
import qualified Text.Pandoc.Builder    as P
import qualified Text.Pandoc.PDF        as Pandoc
import qualified Text.Pandoc.Transforms as Pandoc
import qualified Text.Pandoc.UTF8       as Pandoc
import           Text.Pandoc.Walk       (query)
import qualified Toml
import           Toml                   (TomlCodec, TomlDecodeError, (.=))

data Bulletin doc = Bulletin
  { bulletinTitle         :: !Text
  , bulletinDate          :: !Time.Day
  , bulletinFilename      :: !FilePath
  , bulletinTemplate      :: !FilePath
  , bulletinExtra         :: !(Map Text Text)
  , bulletinContributions :: [Contribution doc]
  } deriving (Show)

type BulletinConfig = Bulletin FilePath

mapBulletin :: (Contribution a -> Contribution b) -> Bulletin a -> Bulletin b
mapBulletin f bulletin = bulletin
  { bulletinContributions = fmap f (bulletinContributions bulletin) }

mapBulletinM :: Monad m => (Contribution a -> m (Contribution b)) -> Bulletin a -> m (Bulletin b)
mapBulletinM f bulletin = do
  contributions <- mapM f $ bulletinContributions bulletin
  pure $ bulletin { bulletinContributions = contributions }

bulletinCodec :: TomlCodec BulletinConfig
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
  } deriving (Show)

type ContributionConfig = Contribution FilePath

mapContribution :: (a -> b) -> Contribution a -> Contribution b
mapContribution f contribution = contribution
  { contributionDocument = f (contributionDocument contribution) }

mapContribution' :: (Contribution a -> b) -> Contribution a -> Contribution b
mapContribution' f contribution = contribution
  { contributionDocument = f contribution }

mapContributionM :: Monad m => (a -> m b) -> Contribution a -> m (Contribution b)
mapContributionM f contribution = do
  doc <- f (contributionDocument contribution)
  pure $ mapContribution (const doc) contribution

contributionCodec :: TomlCodec ContributionConfig
contributionCodec = Contribution
  <$> Toml.text   "author" .= contributionAuthor
  <*> Toml.text   "title"  .= contributionTitle
  <*> Toml.day    "date"   .= contributionDate
  <*> Toml.string "file"   .= contributionDocument

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
    BulletinTomlDecodeError errs -> "Toml error(s): " <> (concat $ intersperse "\n" $ fmap show errs)
    BulletinCompileError err -> "Compile error: " <> Pandoc.toStringLazy err
    BulletinTemplateError err -> "Template error: " <> err
    BulletinUsageError err -> "Usage error: " <> err

newtype BulletinIO a = BulletinIO { unBulletinIO :: ExceptT BulletinError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError BulletinError)

runBulletinIO :: BulletinIO a -> IO a
runBulletinIO mx = do
  res <- runExceptT $ unBulletinIO mx
  case res of
    Left err -> die $ show err
    Right x  -> pure x

liftPandocIO :: PandocIO a -> BulletinIO a
liftPandocIO mx = do
  res <- liftIO $ runIO mx
  case res of
    Left err -> throwError $ BulletinPandocError err
    Right x  -> pure x

-- | Read a contribution and parse it into Pandoc's AST.
readContribution :: Contribution FilePath -> BulletinIO (Contribution Pandoc)
readContribution contribution = do
  let filename = contributionDocument contribution
  let extension = takeExtension filename
  case extension  of
    ".md"   -> do
      content <- liftIO $ Pandoc.readFile filename
      doc <- liftPandocIO $ readMarkdown def content
      pure $ contribution { contributionDocument = doc }
    ".typ"   -> do
      content <- liftIO $ Pandoc.readFile filename
      doc <- liftPandocIO $ readTypst def content
      pure $ contribution { contributionDocument = doc }
    ".docx" -> liftPandocIO $ do
      content <- readFileLazy filename
      doc <- readDocx def content
      pure $ contribution { contributionDocument = doc }
    ".odt" -> liftPandocIO $ do
      content <- readFileLazy filename
      doc <- readODT def content
      pure $ contribution { contributionDocument = doc }
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

-- | Perform all necessary transformations to make an input
-- contribution ready to be used as part of output.
processContribution :: Contribution Pandoc -> Contribution Pandoc
processContribution = mapContribution' addContributionHeader . mapContribution correctHeaderLevels

-- | Read the bulletin configuration from the given file.
readBulletinConfig :: FilePath -> BulletinIO BulletinConfig
readBulletinConfig filename = do
  res <- Toml.decodeFileEither bulletinCodec filename
  case res of
    Left errs      -> throwError $ BulletinTomlDecodeError errs
    Right bulletin -> pure bulletin

-- | Read the contributions specified in the given bulletin
-- configuration.
readContributions :: BulletinConfig -> BulletinIO (Bulletin Pandoc)
readContributions = mapBulletinM readContribution

-- | Process the contributions.
processContributions :: Bulletin Pandoc -> Bulletin Pandoc
processContributions = mapBulletin processContribution

-- | Compile the bulletin, concatenating the contributions together
-- into a single document and setting the required variables.
compileBulletin :: Bulletin Pandoc -> Pandoc
compileBulletin bulletin
  = P.setTitle (P.text $ bulletinTitle bulletin)
  $ P.setDate (P.text $ Text.pack $ Time.showGregorian $ bulletinDate bulletin)
  $ P.setMeta "extra" (bulletinExtra bulletin)
  $ foldMap contributionDocument
  $ bulletinContributions bulletin

-- | Read and compile the template file specified in the bulletin.
readTemplate :: Bulletin Pandoc -> BulletinIO (Template Text)
readTemplate bulletin = do
  templateText <- liftIO $ Text.readFile (bulletinTemplate bulletin)
  templateRes <- liftIO $ compileTemplate (bulletinTemplate bulletin) templateText
  case templateRes of
    Left err    -> throwError $ BulletinTemplateError err
    Right templ -> pure templ

-- | Write the bulletin as a PDF file, compiling it with Typst.
writeBulletin :: Bulletin Pandoc -> Pandoc -> BulletinIO ()
writeBulletin bulletin doc = do
  template <- readTemplate bulletin
  compileRes <- liftPandocIO $
    Pandoc.makePDF
      "typst"
      []
      writeTypst
      def { writerTemplate = Just template }
      doc
  case compileRes of
    Left err  -> throwError $ BulletinCompileError err
    Right out -> liftIO $ BL.writeFile (bulletinFilename bulletin) out

main :: IO ()
main = runBulletinIO $ do
  args <- liftIO getArgs
  when (args == []) $ throwError $ BulletinUsageError "Specify configuration file(s): bulletin <file ...>"
  _ <- for args $ \configFile -> do
    bulletinConfig <- readBulletinConfig configFile
    bulletin <- processContributions <$> readContributions bulletinConfig
    writeBulletin bulletin $ compileBulletin bulletin
  pure ()
