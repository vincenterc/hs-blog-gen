module Main where

import qualified HsBlog
import OptParse
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output env ->
      HsBlog.convertDirectory env input output
    ConvertSingle input output -> do
      (title, inputHandle) <-
        case input of
          Stdin ->
            pure ("", stdin)
          InputFile file ->
            (,) file <$> openFile file ReadMode
      outputHandle <-
        case output of
          Stdout -> pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            shouldOpenFile <-
              if exists
                then confirm
                else pure True
            if shouldOpenFile
              then
                openFile file WriteMode
              else
                exitFailure
      HsBlog.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. use y or n"
      confirm