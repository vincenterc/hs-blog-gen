module HsBlog.Directory
  ( convertDirectory,
    buildIndex,
  )
where

import Control.Exception (SomeException (..), catch, displayException)
import Control.Monad (void, when)
import Control.Monad.Reader (Reader, ask, runReader)
import Data.List (partition)
import Data.Traversable (for)
import HsBlog.Convert (convert, convertStructure)
import HsBlog.Env (Env (..))
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import System.Directory
  ( copyFile,
    createDirectory,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.Exit (exitFailure)
import System.FilePath
  ( takeBaseName,
    takeExtension,
    takeFileName,
    (<.>),
    (</>),
  )
import System.IO (hPutStrLn, stderr)

convertDirectory :: Env -> FilePath -> FilePath -> IO ()
convertDirectory env inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let outputHtmls = runReader (txtsToRenderedHtml filesToProcess) env
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done"

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let (txtFiles, otherFiles) =
        partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <-
    applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $
    DirContents
      { dcFilesToProcess = txtFilesAndContent,
        dcFilesToCopy = otherFiles
      }

data DirContents
  = DirContents
  { -- | File paths and their content
    dcFilesToProcess :: [(FilePath, String)],
    -- | Other file paths, to be copied directly
    dcFilesToCopy :: [FilePath]
  }

buildIndex :: [(FilePath, Markup.Document)] -> Reader Env Html.Html
buildIndex files =
  do
    env <- ask
    let previews =
          map
            ( \(file, doc) ->
                case doc of
                  Markup.Heading 1 heading : article ->
                    Html.h_ 3 (Html.link_ file (Html.txt_ heading))
                      <> foldMap convertStructure (take 2 article)
                      <> Html.p_ (Html.link_ file (Html.txt_ "..."))
                  _ ->
                    Html.h_ 3 (Html.link_ file (Html.txt_ file))
            )
            files
    pure $
      Html.html_
        ( Html.title_ (eBlogName env)
            <> Html.stylesheet_ (eStylesheetPath env)
        )
        ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
            <> Html.h_ 2 (Html.txt_ "Posts")
            <> mconcat previews
        )

txtsToRenderedHtml :: [(FilePath, String)] -> Reader Env [(FilePath, String)]
txtsToRenderedHtml txtFiles = do
  let txtOutputFiles = map toOutputMarkupFile txtFiles
  index <- (,) "index.html" <$> buildIndex txtOutputFiles
  htmlPages <- traverse convertFile txtOutputFiles
  pure $ map (fmap Html.render) (index : htmlPages)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
  (takeBaseName file <.> "html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> Reader Env (FilePath, Html.Html)
convertFile (file, doc) = do
  env <- ask
  pure (file, convert env (takeBaseName file) doc)

createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
      then do
        override <- confirm "Output directory exists. Override?"
        when override (removeDirectoryRecursive dir)
        pure override
      else
        pure True
  when create (createDirectory dir)
  pure create

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures

writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let writeFileContent (file, content) =
        writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action inputs = do
  for inputs $ \input -> do
    maybeResult <-
      catch
        (Right <$> action input)
        ( \(SomeException e) -> do
            pure $ Left (displayException e)
        )
    pure (input, maybeResult)

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]

confirm :: String -> IO Bool
confirm question = do
  putStrLn (question <> " (y/n)")
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. Use y or n."
      confirm question

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()
