{-# LANGUAGE OverloadedStrings #-}
module Code ( generate ) where
import Data.List (partition, intersperse)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

import Types
generate :: Bool -> String -> [Chunk] -> T.Text
generate numberLines ext = expand . merge . (addLineNumbers numberExt) . (filter isDef)
    where
        numberExt = if numberLines then ext else ""
addLineNumbers :: String -> [Chunk] -> [Chunk]
addLineNumbers "" chunks = chunks
addLineNumbers ext chunks = map numberDef chunks
    where
        numberDef (Def (SourceLoc srcFile srcLine) name parts) =
            Def (SourceLoc srcFile srcLine) name ((Code (makeDirective srcFile srcLine)):(addPostRefLineNumbers (makeDirective srcFile) srcLine parts))
        numberDef chunk = chunk
        makeDirective srcFile srcLine = case ext of
            ".c" -> T.pack $ "#line " ++ (show srcLine) ++ " \"" ++ srcFile ++ "\"\n"
            ".hs" -> T.pack $ "{-# LINE " ++ (show srcLine) ++ " \"" ++ srcFile ++ "\" #-}\n"
            _ -> T.pack ""
        addPostRefLineNumbers :: (Int -> T.Text) -> Int -> [Part] -> [Part]
        addPostRefLineNumbers mkDirective cLine ((Ref name indent):(Code c):rest) =
            (Ref name indent):(Code (mkDirective (cLine + 1))):(addPostRefLineNumbers mkDirective (cLine + 2) rest)
        addPostRefLineNumbers mkDirective cLine (part:rest) =
            part:(addPostRefLineNumbers mkDirective (cLine + 1) rest)
        addPostRefLineNumbers mkDirective cLine [] = []

       -- addPostRefLineNumbers mkDirective cLine (part:rest) = case part of
       --     Code c -> (Code c):(addPostRefLineNumbers mkDirective (cLine + 1) rest)
       --     Ref name indent -> (Ref name indent):(Code (mkDirective cLine)):(addPostRefLineNumbers mkDirective (cLine + 1) rest)
merge :: [Chunk] -> [Chunk]
merge = mergeAux []
mergeAux ans [] = ans
mergeAux ans (next:rest) = 
    let 
        name = getName next
        chunkHasName name = (== name) . getName
        (found, rem) = partition (chunkHasName name) rest 
        merged = combineChunks (next:found)
    in 
        mergeAux (merged:ans) rem
combineChunks :: [Chunk] -> Chunk
combineChunks (a:[]) = a
combineChunks l@(c:cs) = Def line name parts 
    where
        parts = concatMap getParts l
        name = getName c
        line = getLineNo c
expand :: [Chunk] -> T.Text
expand chunks =
    let 
        -- map (name, parts)
        partMap = Map.fromList $ zip (map getName chunks) (map getParts chunks)
        backup = getParts $ last chunks
        parts = Map.lookupDefault backup "*" partMap 
    in
        expandParts parts partMap T.empty 
expandParts :: [Part] -> Map.HashMap T.Text [Part] -> T.Text -> T.Text
expandParts parts partMap baseIndent =
    let 
        toText = (\part -> 
            case part of
            Code txt -> T.append baseIndent txt
            Ref name indent -> (expandParts refParts partMap (T.append baseIndent indent))
                where refParts = Map.lookupDefault [] (T.strip name) partMap)
    in 
        T.concat $ map toText parts
