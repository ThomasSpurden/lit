{-# LINE 13 "Markdown.hs.lit" #-}
{-# LINE 20 "Markdown.hs.lit" #-}
{-# LANGUAGE OverloadedStrings #-}
module Markdown ( generate ) where
{-# LINE 26 "Markdown.hs.lit" #-}
import qualified Data.Text as T

import Types
import Highlight (getLang)
{-# LINE 35 "Markdown.hs.lit" #-}
generate :: String -> [Chunk] -> T.Text
generate name chunks = 
    let 
        lang = getLang name
        toMarkDown = chunkToMarkdown lang
    in
        T.concat $ map toMarkDown chunks
{-# LINE 46 "Markdown.hs.lit" #-}
{-# LINE 53 "Markdown.hs.lit" #-}
(<++>) :: T.Text -> T.Text -> T.Text
(<++>) = T.append
{-# LINE 60 "Markdown.hs.lit" #-}
chunkToMarkdown lang chunk =
    case chunk of
    Prose text  -> text
    Def _ name parts -> 
        let 
            lang' = T.pack lang
            header = "<< " <++> (T.strip name) <++> " >>="
            mdParts = T.concat $ map (partToText lang) parts
        in 
            "```" <++> lang'   <++> 
            "\n"  <++> header  <++> 
            "\n"  <++> mdParts <++> "```\n"
{-# LINE 79 "Markdown.hs.lit" #-}
partToText :: String -> Part -> T.Text
partToText lang part =
    case part of
    Code txt -> txt
    Ref txt indent -> (indent <++> "<< " <++> (T.strip txt) <++> " >>\n")
