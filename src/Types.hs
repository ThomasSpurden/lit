{-# LINE 7 "Types.hs.lit" #-}
{-# LINE 14 "Types.hs.lit" #-}
module Types where

import Data.Text 
{-# LINE 26 "Types.hs.lit" #-}
data SourceLoc = SourceLoc String Int deriving (Show, Eq)
data Chunk = Def SourceLoc Text [Part] | Include String Text | Prose Text deriving (Show, Eq)
data Part = Code Text | Ref Text Text deriving (Show, Eq)
type Program = [Chunk]
{-# LINE 33 "Types.hs.lit" #-}
isDef chunk =
    case chunk of
    Def _ _ _ -> True
    Prose _ -> False
{-# LINE 40 "Types.hs.lit" #-}
isRef part =
    case part of
    Ref _ _ -> True
    _ -> False
{-# LINE 47 "Types.hs.lit" #-}
getName chunk =
    case chunk of
    Def _ name _ -> name
    _ -> error "cannot retrieve name, not a def"
{-# LINE 54 "Types.hs.lit" #-}
getCodeText part = 
    case part of
    Code txt -> txt
    _ -> error "cannot retrieve text, not a code part"
{-# LINE 62 "Types.hs.lit" #-}
getParts chunk =
    case chunk of
    Def _ _ parts -> parts
    _ -> error "cannot retrieve parts, not a def"
{-# LINE 70 "Types.hs.lit" #-}
getLineNo chunk =
    case chunk of
    Def line _ _ -> line
    _ -> error "cannot retrieve line number, not a def"
{-# LINE 77 "Types.hs.lit" #-}
getProseText chunk = 
    case chunk of
    Prose txt -> txt
    _ -> error "cannot retrieve text, not a prose"
