module Convert where

import Html qualified
import Markup qualified

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n txt
    Markup.Paragraph p ->
      Html.p_ p
    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list
    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list
    Markup.CodeBlock list ->
      Html.code_ (unlines list)
