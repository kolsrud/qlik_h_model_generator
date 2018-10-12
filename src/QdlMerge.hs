module QdlMerge (
  mergeQdlFiles
) where

import QixAbsSyntax
import Data.List(nub, intersect, sort, partition)
import PruneQix (pruneQixDefinition)
--import Debug.Trace

mergeQdlFiles :: QixFile -> QixFile -> QixFile
mergeQdlFiles target source =
  let mergeAll = (mergeTypes source).(mergeClasses source)
   in mergeAll target

mergeClasses :: QixFile -> QixFile -> QixFile
mergeClasses source target =
  let extendedClasses = map (getClass source) (getAllExtendingClasses target)
   in foldl mergeExtendedClass target extendedClasses

mergeExtendedClass :: QixFile -> ClassDefinition -> QixFile
mergeExtendedClass (QixFile m_lib defs) extendedClass@(ClassDef _ Nothing (Just extensionMethods)) =
  let classId = getClassId extendedClass
      (extendingClasses, otherDefs) = partition ((extendsClass classId).snd) defs
   in case extendingClasses of
        [] -> error $ "Could not find extended class " ++ show classId ++ " in source file."
        [(attribs, QixDef_Extend classDef)] ->
          let ClassDef _ Nothing (Just methods) = classDef
              newClass = QixDef_Extend (ClassDef classId Nothing (Just $ methods ++ extensionMethods))
              newDefs = (pruneQixDefinition (attribs, newClass)):otherDefs
           in QixFile m_lib newDefs
        _  -> error $ "Multiple definitions of extended class " ++ show classId ++ " in source file."

extendsClass :: String -> QixDefinition -> Bool
extendsClass classId qixDef = case qixDef of
  (QixDef_Extend classDef) -> getClassId classDef == classId
  _ -> False

getClassId :: ClassDefinition -> String
getClassId (ClassDef classId _ _) = classId

getClass :: QixFile -> Id -> ClassDefinition
getClass (QixFile _ defs) classId = case [ classDef | QixDef_Class classDef@(ClassDef _ _ (Just _)) <- map snd defs, classId == (getClassId classDef) ] of
  []    -> error $ "Could not find extended class " ++ show classId ++ " in source file."
  [def] -> def
  _     -> error $ "Multiple definitions of extended class " ++ show classId ++ " in source file."

getAllExtendingClasses :: QixFile -> [String]
getAllExtendingClasses (QixFile _ defs) = [ getClassId classDef | QixDef_Extend classDef <- map snd defs ]
  
mergeTypes :: QixFile -> QixFile -> QixFile
mergeTypes source target =
  let allDeclaredSourceTypes   = sort $ getAllDeclaredTypeIds source
      allReferencedTargetTypes = sort $ getAllReferencedTypeIds target
      typesToMove = sort $ nub $ allDeclaredSourceTypes `intersect` allReferencedTargetTypes
   in if (null typesToMove)
      then target
      else let (newSource, newTarget) = foldl moveType (source,target) typesToMove
            in mergeTypes newSource newTarget

moveType :: (QixFile, QixFile) -> Id -> (QixFile, QixFile)
moveType (source, target) typeId =
  let qixDefinition = getTypeDefinition typeId source
   in (dropTypeDefinition typeId source, addQixDefinition qixDefinition target)

getTypeDefinition :: Id -> QixFile -> ([Attribute], QixDefinition)
getTypeDefinition typeId (QixFile _ defs) =
  let matchingTypes = filter ((isTypeDefWithId typeId).snd) defs
   in case matchingTypes of
        [] -> error $ "No type found matching identifier: " ++ typeId
        [def] -> def
        _ -> error $ "Multiple types found matching identifier: " ++ typeId

getAllTypeDefinitions :: QixFile -> [TypeDefinition]
getAllTypeDefinitions (QixFile _ defs) = [ typeDef | QixDef_Type typeDef <- map snd defs ]

getTypeDefId :: TypeDefinition -> Id
getTypeDefId typeDefinition = case typeDefinition of
  TypeDef_Enum   (EnumDef id _) -> id
  TypeDef_Struct (StructDef id _) -> id
  TypeDef_Alias  (TypeAliasDef id _) -> id

dropTypeDefinition :: Id -> QixFile -> QixFile
dropTypeDefinition typeId (QixFile m_lib defs) =
  QixFile m_lib (filter (not.(isTypeDefWithId typeId).snd) defs)

isTypeDefWithId :: Id -> QixDefinition -> Bool
isTypeDefWithId typeId qixDef = case qixDef of
  QixDef_Type typeDef -> getTypeDefId typeDef == typeId
  _ -> False

addQixDefinition :: ([Attribute], QixDefinition) -> QixFile -> QixFile
addQixDefinition qixDef (QixFile m_lib defs) = QixFile m_lib (qixDef:defs)

getAllDeclaredTypeIds :: QixFile -> [Id]
getAllDeclaredTypeIds qixFile = map getTypeDefId (getAllTypeDefinitions qixFile)

getAllReferencedTypeIds :: QixFile -> [Id]
getAllReferencedTypeIds (QixFile _ defs) = concatMap (getAllReferencedTypeIdsQidDef.snd) defs

getAllReferencedTypeIdsQidDef :: QixDefinition -> [Id]
getAllReferencedTypeIdsQidDef def = case def of
  QixDef_Rule      _   -> []
  QixDef_Class     def -> getAllReferencedTypeIdsClassDef def
  QixDef_Extend    def -> getAllReferencedTypeIdsClassDef def
  QixDef_Interface def -> getAllReferencedTypeIdsClassDef def
  QixDef_Type      def -> getAllReferencedTypeIdsTypeDef def
  QixDef_Include   _   -> []

getAllReferencedTypeIdsClassDef :: ClassDefinition -> [Id]
getAllReferencedTypeIdsClassDef (ClassDef _ m_ids m_methods) =
   concat m_ids ++
   concat (fmap (concatMap getAllReferencedTypeIdsMethod) m_methods)

getAllReferencedTypeIdsMethod :: MethodDefinition -> [Id]
getAllReferencedTypeIdsMethod (MethodDef _ _ returnType args _) =
   getAllReferencedTypeIdsTypeRef returnType ++
   concatMap getAllReferencedTypeIdsMethodArg args

getAllReferencedTypeIdsMethodArg :: MethodArgumentDefinition -> [Id]
getAllReferencedTypeIdsMethodArg (MethodArgumentDef _ _ typeRef) = getAllReferencedTypeIdsTypeRef typeRef

getAllReferencedTypeIdsTypeRef :: TypeRef -> [Id]
getAllReferencedTypeIdsTypeRef typeRef = case typeRef of
  TypeRef_Primitive _   -> []
  TypeRef_Array     ref -> getAllReferencedTypeIdsTypeRef ref
  TypeRef_Ref       ref -> getAllReferencedTypeIdsTypeRef ref
  TypeRef_Name      id  -> [id]

getAllReferencedTypeIdsTypeDef :: TypeDefinition -> [Id]
getAllReferencedTypeIdsTypeDef def = case def of
  TypeDef_Enum   _ -> []
  TypeDef_Struct (StructDef _ structMembers) -> concatMap getAllReferencedTypeIdsTypeRef [ ref | StructMemberDef _ _ ref _ <- structMembers ]
  TypeDef_Alias  (TypeAliasDef _ ref) -> getAllReferencedTypeIdsTypeRef ref
