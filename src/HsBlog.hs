module HsBlog
  ( convertSingle,
    convertDirectory,
    process,
    buildIndex,
  )
where

import HsBlog.Convert (convert)
import HsBlog.Directory (buildIndex, convertDirectory)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

-- whenIO :: IO Bool -> IO () -> IO ()
-- whenIO cond action = do
--   result <- cond
--   if result
--     then action
--     else pure ()