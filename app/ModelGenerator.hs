import QixParser(runParse)
import PruneQix(prune)
import QixAbsSyntax
import System.Directory (createDirectoryIfMissing)
import Data.Maybe
import Data.Either(lefts, rights)
import Data.List(sort, nub)
import QdlMerge
import QixEnum
import QixClasses
import Debug.Trace

is :: (a -> b) -> (b -> Bool) -> a -> Bool
is f p = p.f

main :: IO ()
main = do
  m_results <- parseFiles $ map ("qdlFiles/"++) ["qix.qdl", "qvnext.qdl"]
  case m_results of
    Nothing -> return ()
    Just [qix, qvnext] -> do
      let qvNext = prune $ mergeQdlFiles qvnext qix
      let (props, dataStructures) = makeDataStructures qvNext
      let enumTypes = makeEnumTypes qvNext
      let (classProps, qixClasses, classes) = makeClasses qvNext
      writePropClasses (classProps ++ props)
      let header = [ "{-# LANGUAGE FlexibleInstances      #-}"
                   , "{-# LANGUAGE TypeSynonymInstances   #-}"
                   , "{-# LANGUAGE FunctionalDependencies #-}"
                   , "{-# LANGUAGE MultiParamTypeClasses  #-}"
                   , "module ModelEngine where"
                   , "import Control.Lens"
                   , "import AbstractStructure"
                   , "import HardCoded"
                   , "import SDKMonad"
                   , "import SDKBase"
                   , "import PropClasses"
                   ]
      let modelEngineContents = concatMap unlines $ [ header ] ++ dataStructures ++ enumTypes ++ [qixClasses]
      writeHsFile "ModelEngine" modelEngineContents
      mapM_ writeClass classes

writePropClasses :: [Id] -> IO ()
writePropClasses propIds = do
  let uniquePropNames = sort $ nub propIds 
      propClasses = map makeHasPropClass uniquePropNames
      header = [ "{-# LANGUAGE FunctionalDependencies #-}"
               , "{-# LANGUAGE MultiParamTypeClasses  #-}"
               , "module PropClasses where"
               , "import Control.Lens"
               , "import AbstractStructure"
               ]
   in writeHsFile "PropClasses" (unlines $ header ++ propClasses)

writeClass :: (String, [String]) -> IO ()
writeClass (classId, txt) = do
  let header = [ "{-# LANGUAGE FlexibleInstances      #-}"
               , "{-# LANGUAGE TypeSynonymInstances   #-}"
               , "{-# LANGUAGE FunctionalDependencies #-}"
               , "{-# LANGUAGE MultiParamTypeClasses  #-}"
               , "module " ++ classId ++ " where"
               , ""
               , "import Text.JSON"
               , "import Control.Lens"
               , "import AbstractStructure"
               , "import HardCoded"
               , "import SDKMonad"
               , "import SDKBase"
               , "import PropClasses"
               , "import ModelEngine"
               ]
  writeHsFile classId (unlines $ header ++ txt)

writeHsFile :: String -> String -> IO ()
writeHsFile fileName txt = do
  let outputDir = "GeneratedFiles"
  createDirectoryIfMissing False outputDir
  let fullFileName = outputDir ++ "/" ++ fileName ++ ".hs"
  putStrLn $ "Writing file " ++ fullFileName
  writeFile fullFileName txt

parseFiles :: [String] -> IO (Maybe [QixFile])
parseFiles fileNames = do
  parseResults <- mapM runParse fileNames
  let errors = lefts parseResults
  if (null errors)
   then return (Just $ rights parseResults)
   else mapM print errors >> return Nothing

makeDataStructures :: QixFile -> ([Id], [[String]])
makeDataStructures qixFile =
  let allStructs   = dropIntStruct $ getAllStructs qixFile
      allStructIds = map getStructId allStructs
      allProps     = concatMap getAllProperties allStructs
   in ( map (fst.snd) allProps
      , [ map makeNewType allStructIds
        , map makeShowInstance allStructIds
        , map makeContainerInstance allStructIds
        , map makeValueTypeInstance allStructIds
--        , map makeHasPropClass uniquePropNames
        , map makeHasPropInstance allProps
        , map makeTypeAlias (getAllTypeAlias qixFile)
        ]
      )
 where
  getStructId (StructDef id _) = id

  dropIntStruct = filter (getStructId `is` (/= "Int"))

getAllStructs :: QixFile -> [StructDefinition]
getAllStructs (QixFile _ defs) = catMaybes (map getStructDefinition defs)
 where
  getStructDefinition :: ([Attribute], QixDefinition) -> Maybe StructDefinition
  getStructDefinition (_, QixDef_Type (TypeDef_Struct struct)) = Just struct
  getStructDefinition _ = Nothing

getAllProperties :: StructDefinition -> [(Id, (Id, TypeRef))]
getAllProperties (StructDef structId members) = zip (repeat structId) (map getStructMember members)
 where
  getStructMember :: StructMemberDefinition -> (Id, TypeRef)
  getStructMember (StructMemberDef id _ typeRef _) = (id, typeRef)

-- makeNewType :: StructDefinition -> String
-- makeNewType (StructDef id _) =
--   "newtype " ++ id ++ " = " ++ id ++ " AbstractStructure"

-- makeShowInstance :: StructDefinition -> String
-- makeShowInstance (StructDef id _) =
--   "instance Show " ++ id ++ " where show = show.toAs"

-- makeContainerInstance :: StructDefinition -> String
-- makeContainerInstance (StructDef id _) =
--   "instance AbstractStructureContainer " ++ id ++ " where\n" ++
--   "  toAs (" ++ id ++ " as) = as\n" ++
--   "  fromAs as = " ++ id ++ " as\n"

-- makeValueTypeInstance :: StructDefinition -> String
-- makeValueTypeInstance (StructDef id _) =
--   "instance ValueType " ++ id ++ " where\n" ++
--   "  toValue (" ++ id ++ " as) = toValue as\n" ++
--   "  fromValue as = " ++ id ++ " (fromValue as)\n"

makeHasPropClass :: Id -> String
makeHasPropClass propName = unlines $
  [ "class " ++ makeHasName propName ++ " a b | a -> b where"
  , "  q" ++ propName ++ " :: (AbstractStructureContainer a, ValueType b) => Lens' a b"
  , "  q" ++ propName ++ " = cp_ \"q" ++ propName ++ "\""
  ]

getAllTypeAlias :: QixFile -> [TypeAliasDefinition]
getAllTypeAlias (QixFile _ defs) = [ def | QixDef_Type (TypeDef_Alias def) <- map snd defs ]

makeTypeAlias :: TypeAliasDefinition -> String
makeTypeAlias aliasDef@(TypeAliasDef typeId ref) = case ref of
  TypeRef_Array innerRef -> case innerRef of
    TypeRef_Name innerTypeId -> unwords ["type", typeId, "=", "[", innerTypeId, "]"]
    TypeRef_Primitive innerType -> unwords ["type", typeId, "=", "[", toHaskellPrimType innerType, "]"]
    _ -> error $ "makeTypeAlias : " ++ show aliasDef
  _ -> error $ "makeTypeAlias : " ++ show aliasDef
