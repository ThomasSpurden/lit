{-# LINE 17 "Parse.hs.lit" #-}
{-# LINE 26 "Parse.hs.lit" #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse where
{-# LINE 31 "Parse.hs.lit" #-}
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T

import Types
{-# LINE 39 "Parse.hs.lit" #-}
encode :: T.Text -> String -> [Chunk]
encode txt fileName =
    case (parse entire fileName txt) of
    Left err -> []
    Right result -> result
{-# LINE 47 "Parse.hs.lit" #-}
{-# LINE 62 "Parse.hs.lit" #-}
entire :: Parser Program
entire = manyTill chunk eof
{-# LINE 67 "Parse.hs.lit" #-}
chunk :: Parser Chunk
chunk = (try def) <|> prose
{-# LINE 72 "Parse.hs.lit" #-}
prose :: Parser Chunk
prose = grabLine >>= (\line -> return $ Prose line)
{-# LINE 78 "Parse.hs.lit" #-}
def :: Parser Chunk
def = do
    (indent, header, pos) <- title
    parts <- manyTill (part indent) $ endDef indent
    return $ Def (SourceLoc (sourceName pos) (sourceLine pos)) header parts
{-# LINE 89 "Parse.hs.lit" #-}
endDef :: String -> Parser ()
endDef indent = try $ do { skipMany newline; notFollowedBy (string indent) <|> (lookAhead title >> parserReturn ()) }
{-# LINE 95 "Parse.hs.lit" #-}
-- Returns (indent, macro-name, line-no)
title :: Parser (String, T.Text, SourcePos)
title = do
    pos <- getPosition
    indent <- many ws
    name <- packM =<< between (string "<<") (string ">>=") (many notDelim)
    newline
    return $ (indent, T.strip name, pos)
{-# LINE 106 "Parse.hs.lit" #-}
notDelim = noneOf ">="
{-# LINE 111 "Parse.hs.lit" #-}
part :: String -> Parser Part
part indent = 
    try (string indent >> varLine) <|> 
    try (string indent >> defLine) <|>
    (grabLine >>= \extra -> return $ Code extra)
{-# LINE 119 "Parse.hs.lit" #-}
varLine :: Parser Part
varLine = do
    indent <- packM =<< many ws
    name <- packM =<< between (string "<<") (string ">>") (many notDelim)
    newline
    return $ Ref name indent
{-# LINE 128 "Parse.hs.lit" #-}
defLine :: Parser Part
defLine = do
    line <- grabLine
    return $ Code line
{-# LINE 135 "Parse.hs.lit" #-}
grabLine :: Parser T.Text
grabLine = do 
    line <- many (noneOf "\n\r")
    last <- newline
    return $ T.pack $ line ++ [last]
{-# LINE 143 "Parse.hs.lit" #-}
ws :: Parser Char
ws = char ' ' <|> char '\t'
{-# LINE 148 "Parse.hs.lit" #-}
packM str = return $ T.pack str
{-# LINE 152 "Parse.hs.lit" #-}
textP :: Parsec T.Text () T.Text ->  T.Text -> T.Text
textP p txt =
    case (parse p "" txt) of 
    Left err -> T.empty
    Right result -> result

chunkP :: Parsec T.Text () Chunk ->  T.Text -> Maybe Chunk
chunkP p txt =
    case (parse p "" txt) of 
    Left err -> Nothing
    Right result -> Just result
