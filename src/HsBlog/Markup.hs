module HsBlog.Markup
  ( Document,
    Structure (..),
    parse,
  )
where

import Data.Maybe (maybeToList)
import Numeric.Natural

type Document =
  [Structure]

-- | Represents a single markup structure. Such as:
--
-- - A paragraph
-- - An unordered list
-- - A code block
data Structure
  = -- | A section heading with a level
    Heading Natural String
  | -- | A paragraph
    Paragraph String
  | -- | An unordered list of strings
    UnorderedList [String]
  | -- | An ordered list of strings
    OrderedList [String]
  | -- | A code block
    CodeBlock [String]
  deriving (Eq, Show)

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          parseLines (Just (CodeBlock (code <> [line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)
    currentLine : rest ->
      let line = trim currentLine
       in if line == ""
            then
              maybe id (:) context (parseLines Nothing rest)
            else case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
