module PruneQix
  ( prune
  , pruneQixDefinition
  ) where

--import QixParser(runParse)
import QixAbsSyntax
import Debug.Trace
import Data.List

{-
qixFiles = ["qix.qdl", "qvnext.qdl"]

main :: IO ()
main = do
  qvNextResult <- runParse "qvnext.qdl"
  case qvNextResult of
    Left err -> print err
    Right qvNext -> do let newQix = prune qvNext
                       let orig = show qvNext
                       let new = show newQix
                       let sizeDiff = (length new)- (length orig)
                       putStrLn ("Size diff: " ++ show sizeDiff)
                       writeFile "pruned.qdl" new
                       writeFile "prunedPretty.qdl" (printQix newQix)
-}

prune :: QixFile -> QixFile
prune (QixFile _ definitions) =
  let publicDefinitions = filter isPublicQixDefinition definitions
   in QixFile Nothing (map pruneQixDefinition publicDefinitions)

isPublicQixDefinition :: ([Attribute], QixDefinition) -> Bool
isPublicQixDefinition (attribs, definition) = case definition of
  QixDef_Rule      _ -> False
  QixDef_Class     _ -> any isPublicJson attribs
  QixDef_Extend    _ -> True
  QixDef_Interface _ -> False
  QixDef_Type      _ -> True
  QixDef_Include   _ -> True

isPublicJson :: Attribute -> Bool
isPublicJson = not.isInternalJson

-- isPublicJson (Attribute "json" False args) = False
-- isPublicJson (Attribute "json" _ args) = not (any isInternalTest args)
--  where
--   isInternalTest :: AttributeArgument -> Bool
--   isInternalTest (AttributeArgument_Expr (Just "internal_test") _ _) = True
--   isInternalTest _ = False
-- isPublicJson _ = False

isInternalJson :: Attribute -> Bool
isInternalJson (Attribute "json" True  _) = True
isInternalJson (Attribute "json" False args) = any isJsonInternalArg args
isInternalJson _ = False
             
pruneQixDefinition (attribs, QixDef_Extend classDef) = (attribs, QixDef_Extend (pruneQixDefClass (any isJson attribs) classDef))
pruneQixDefinition (attribs, QixDef_Class classDef) = (attribs, QixDef_Class (pruneQixDefClass (any isJson attribs) classDef))
pruneQixDefinition def = def

pruneQixDefClass :: Bool -> ClassDefinition -> ClassDefinition
pruneQixDefClass _ def@(ClassDef _ _ Nothing) = def
pruneQixDefClass isPublicDefault (ClassDef id m_ids (Just methods)) =
  ClassDef id m_ids (Just (filter (isPublicMethod isPublicDefault) methods))

getMethodId :: MethodDefinition -> String
getMethodId (MethodDef methodId _ _ _ _) = methodId
            
isPublicMethod :: Bool -> MethodDefinition -> Bool
isPublicMethod isPublicDefault (MethodDef _ attribs _ _ _) = not (any isJsonInternal attribs) && (isPublicDefault || any isJson attribs)

isJson :: Attribute -> Bool
isJson (Attribute "json" _ _) = True
isJson _ = False

isJsonInternal :: Attribute -> Bool
isJsonInternal (Attribute "json" False _) = True
isJsonInternal (Attribute "json" _ args) = any isJsonInternalArg args
isJsonInternal _ = False

isJsonInternalArg :: AttributeArgument -> Bool
isJsonInternalArg (AttributeArgument_Expr (Just id) _ _) = "internal" `isPrefixOf` id
isJsonInternalArg (AttributeArgument_Class id) = "internal" `isPrefixOf` id
isJsonInternalArg _ = False
