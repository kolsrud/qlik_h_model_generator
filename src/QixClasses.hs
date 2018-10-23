module QixClasses
 ( makeClasses
 , makeNewType
 , makeShowInstance
 , makeContainerInstance
 , makeValueTypeInstance
 , makeHasPropInstance
 , makeHasName
 , getTargetType
 , getTargetPrimType
 , toHaskellPrimType
 ) where

import QixAbsSyntax
import Data.Maybe
import Data.Char
import Data.List
import Debug.Trace
              
makeClasses :: QixFile -> ([String], [String], [(String, [String])])
makeClasses qixFile = let (props, qixClasses, classes) = unzip3 $ map makeClass (getAllClasses qixFile)
                       in (concat props, concat qixClasses, classes)

makeClass :: ClassDefinition -> ([String], [String], (String, [String]))
makeClass cls@(ClassDef classId _ _) =
  let (props, methods) = unzip $ makeMethods cls
   in ( concat props
      , makeQixClass classId
      , ( classId
        ,    makeClassType cls
          ++ methods
        )
      )

makeQixClass :: Id -> [String]
makeQixClass classId =
 [ unwords [ "newtype", classId, "=", classId, "QixObject" ]
 , makeValueTypeInstance classId
 ]

getAllClasses :: QixFile -> [ClassDefinition]
getAllClasses (QixFile _ defs) = catMaybes (map (getClass.snd) defs)

getClass :: QixDefinition -> Maybe ClassDefinition
getClass qixDef = case qixDef of
  QixDef_Class     c -> Just c
  QixDef_Extend    c -> Just c
  QixDef_Interface c -> Just c
  _ -> Nothing
                     
makeClassType :: ClassDefinition -> [String]
makeClassType (ClassDef classId _ _) =
  [ "instance QixClass " ++ classId ++ " where"
  , "  getHandle (" ++ classId ++ " (QixObject h _)) = h"
  , ""
  ]

makeValueTypeInstance :: Id -> String
makeValueTypeInstance id =
  "instance ValueType " ++ id ++ " where\n" ++
  "  toValue (" ++ id ++ " as) = toValue as\n" ++
  "  fromValue as = " ++ id ++ " (fromValue as)\n"

-- makeJSONInstance :: ClassDefinition -> String
-- makeJSONInstance (ClassDef classId _ _) = unlines $
--   [ "instance JSON " ++ classId ++ " where"
--   , "  readJSON v = case (readJSON v)::(Result QixObject) of"
--   , "    Error e -> Error $ \"Unable to parse " ++ classId ++ ". \" ++ e ++ \": \" ++ show v"
--   , "    Ok q -> Ok (" ++ classId ++ " q)"
--   , "  showJSON (" ++ classId ++ " q) = showJSON q"
--   ]

makeMethods :: ClassDefinition -> [([String], String)]
makeMethods (ClassDef classId _ m_methods) = map (makeMethod classId) (concat m_methods)

makeMethod :: Id -> MethodDefinition -> ([String], String)
makeMethod classId method@(MethodDef methodId attribs retType args _) =
  let (inputArgs, outputArgs) = partition isInputArg args
      (retProps, returnType)  = makeReturnType methodId retType outputArgs
      returnTypeString        = toHaskellType returnType
      qixMethodId             = maybe methodId getAlias (getAttribute "json_alias" attribs)
   in ( map fst retProps
      , unlines $
          [ makeSyncMethod classId methodId inputArgs returnType
          ] ++
          ( if null retProps
            then []
            else [ makeNewType           returnTypeString
                 , makeShowInstance      returnTypeString
                 , makeValueTypeInstance returnTypeString
                 , makeContainerInstance returnTypeString
                 ] ++
                 ( map makeHasPropInstance (zip (repeat returnTypeString) (retProps)) ) ++
                 [ "" ]
          ) ++
          [ makeAsyncMethod classId methodId qixMethodId (inputArgs, outputArgs) retType returnType
          ]
      )

getAlias :: Attribute -> String
getAlias attribute = case attribute of
  Attribute "json_alias" True [AttributeArgument_Expr (Just alias) Nothing Nothing] -> alias
  _ -> error $ "getAlias " ++ show attribute

getArgId (MethodArgumentDef argId _ _) = argId

makeSyncMethod :: String -> String -> [MethodArgumentDefinition] -> TypeRef -> String 
makeSyncMethod classId methodId inputArgs returnType =
  let syncMethodId   = camelCase methodId
      syncMethodId_  = camelCase methodId ++ "_"
      asyncMethodId  = syncMethodId ++ "Async"
      asyncMethodId_ = syncMethodId ++ "Async_"
      (mandatory,optional) = break isOptionalArg inputArgs
   in unlines $
        [ makeSyncTypeSignature classId syncMethodId mandatory returnType
        , unwords $ [call syncMethodId mandatory, "=", call asyncMethodId mandatory, ">>= awaitResult"]
        ] ++
        ( if null optional
          then []
          else [ ""
               , makeSyncTypeSignature classId syncMethodId_ inputArgs returnType
               , unwords $ [call syncMethodId_ inputArgs, "=", call asyncMethodId_ inputArgs, ">>= awaitResult"]
               ]
        )

syncMethodId, asyncMethodId :: String -> String
syncMethodId  = camelCase
asyncMethodId =  (++"Async").syncMethodId

call :: String -> [MethodArgumentDefinition] -> String
call methodId args =
  let inputArgIds = map (camelCase.getArgId) args
   in unwords (methodId:"obj":inputArgIds)

makeSyncTypeSignature :: String -> String -> [MethodArgumentDefinition] -> TypeRef -> String
makeSyncTypeSignature classId methodId inputArgs returnType =
  let haskellArgs   = intersperse "->" (classId:(map makeHaskellArg inputArgs))
   in methodId ++ " :: " ++ (unwords haskellArgs) ++ " -> SDKM " ++ toHaskellType returnType

makeAsyncTypeSignature :: String -> String -> [MethodArgumentDefinition] -> TypeRef -> String
makeAsyncTypeSignature classId methodId inputArgs returnType =
  let haskellArgs   = intersperse "->" (classId:(map makeHaskellArg inputArgs))
   in methodId ++ " :: " ++ (unwords haskellArgs) ++ " -> SDKM (Task " ++ toHaskellType returnType ++ ")"

makeAsyncMethod :: String -> String -> String -> ([MethodArgumentDefinition], [MethodArgumentDefinition]) -> TypeRef -> TypeRef -> String 
makeAsyncMethod classId methodId qixMethodId (inputArgs, outputArgs) retType returnType =
  let asyncMethodId  = camelCase methodId ++ "Async"
      asyncMethodId_ = asyncMethodId ++ "_"
      inputArgIds    = "obj":(map (camelCase.getArgId) inputArgs)
      callbackMethod = "(" ++ makeCallbackMethodCall methodId (not $ isVoid retType) (map getArgId outputArgs) ++ ")"
      (mandatory,optional) = break isOptionalArg inputArgs
   in unlines $
          [ makeAsyncTypeSignature classId asyncMethodId mandatory returnType
          , unwords $ [call asyncMethodId mandatory, "="]
          , defineArgs mandatory $ \args -> unwords $ ["sendRequestM (getHandle obj)", show qixMethodId, args, callbackMethod]
          ] ++
          ( if null optional
            then []
            else [ ""
                 , makeAsyncTypeSignature classId asyncMethodId_ inputArgs returnType
                 , unwords $ [call asyncMethodId_ inputArgs, "="]
                 , defineArgs inputArgs $ \args -> unwords $ ["sendRequestM (getHandle obj)", show qixMethodId, args, callbackMethod]
                 ]
          )

defineArgs :: [MethodArgumentDefinition] -> (String -> String) -> String
defineArgs []   f = "  " ++ f "[]"
defineArgs args f = "  let args = [" ++ intercalate ", " (map makeArgumentDefinition args) ++ "]\n" ++ 
                    "   in " ++ f "args"

makeNewType :: Id -> String
makeNewType id =
  "newtype " ++ id ++ " = " ++ id ++ " AbstractStructure"

makeShowInstance :: Id -> String
makeShowInstance id =
  "instance Show " ++ id ++ " where show = show.toAs"

makeContainerInstance :: Id -> String
makeContainerInstance id =
  "instance AbstractStructureContainer " ++ id ++ " where\n" ++
  "  toAs (" ++ id ++ " as) = as\n" ++
  "  fromAs as = " ++ id ++ " as"

makeHasPropInstance :: (Id, (Id, TypeRef)) -> String
makeHasPropInstance (structName, (propName, typeRef)) =
  dropWhile (==' ') $
  unwords [ if ('?' `elem` targetType) then "--" else ""
          , "instance", makeHasName propName, structName, targetType
          ]
 where
  targetType = getTargetType typeRef

makeArgumentDefinition :: MethodArgumentDefinition -> String
makeArgumentDefinition (MethodArgumentDef aId _ _) = "(\"q" ++ aId ++ "\", toValue " ++ camelCase aId ++ ")"

makeCallbackMethodCall :: String -> Bool -> [String] -> String
makeCallbackMethodCall methodId hasReturn outArgs = case (hasReturn, outArgs) of
  (True, [])   -> unwords $ ["onReturnValueResponse", show methodId]
  (False, [p]) -> unwords $ ["onSingleValueResponse", show methodId, show ('q':p)]
  _            -> unwords $ ["onMultiValueResponse", show methodId]

        
isVoid :: TypeRef -> Bool
isVoid (TypeRef_Primitive PrimitiveType_void) = True
isVoid _ = False

makeReturnType :: String -> TypeRef -> [MethodArgumentDefinition] -> ([(String, TypeRef)], TypeRef)
makeReturnType methodId retType outArgs =
 let outProperties = map makeReturnProperty outArgs
     returnProperties = if isVoid retType
                        then outProperties
                        else ("Return", retType):outProperties
  in case returnProperties of
       []  -> ([], TypeRef_Primitive PrimitiveType_void)
       [p] -> ([], snd p)
       _   -> let returnTypeName = methodId ++ "Result"
               in (returnProperties, TypeRef_Name returnTypeName)

makeReturnProperty :: MethodArgumentDefinition -> (String, TypeRef)
makeReturnProperty (MethodArgumentDef argId _ typeRef) = (argId, typeRef)

camelCase :: String -> String
camelCase "" = ""
camelCase (c:cs) = (toLower c):cs

toHaskellType :: TypeRef -> String
toHaskellType typeRef = case typeRef of
  TypeRef_Primitive primType -> toHaskellPrimType primType
  TypeRef_Array     typeRef  -> let innerType = toHaskellType typeRef
                                 in "[" ++ innerType ++ "]"
  TypeRef_Ref       typeRef  -> toHaskellType typeRef
  TypeRef_Name      typeName -> typeName

toHaskellPrimType :: PrimitiveType -> String
toHaskellPrimType primType = case primType of
  PrimitiveType_void   -> "()"
  PrimitiveType_bool   -> "Bool"
  PrimitiveType_byte   -> "Int"
  PrimitiveType_int    -> "Int"
  PrimitiveType_int64  -> "Integer"
  PrimitiveType_uint64 -> "Integer"
  PrimitiveType_float  -> error (show primType)
  PrimitiveType_double -> "Double"
  PrimitiveType_char   -> "Char"
  PrimitiveType_string -> "String"

getAttribute :: String -> [Attribute] -> Maybe Attribute
getAttribute _ [] = Nothing
getAttribute attribId (a@(Attribute theId _ _):as) = if attribId == theId then Just a else getAttribute attribId as
        
hasAttributeArg :: String -> MethodArgumentDefinition -> Bool
hasAttributeArg attrib (MethodArgumentDef _ attribs _) = any (==attrib) [ attribId | Attribute attribId _ _ <- attribs]

isInputArg :: MethodArgumentDefinition -> Bool
isInputArg = hasAttributeArg "in"

isOptionalArg :: MethodArgumentDefinition -> Bool
isOptionalArg = hasAttributeArg "optional"

makeHaskellArg :: MethodArgumentDefinition -> String
makeHaskellArg argDef@(MethodArgumentDef _ _ typeRef) = if isOptionalArg argDef
--                                                        then "Maybe " ++ toHaskellType typeRef
                                                        then toHaskellType typeRef
                                                        else toHaskellType typeRef

getTargetType :: TypeRef -> String
getTargetType typeRef = case typeRef of
  TypeRef_Primitive primType -> getTargetPrimType primType
  TypeRef_Array _typeRef -> let innerType = getTargetType _typeRef
                             in "[" ++ innerType ++ "]"
  TypeRef_Ref _typeRef -> "?"
  TypeRef_Name typeName -> typeName

makeHasName :: Id -> String
makeHasName propName = "HasQ" ++ propName

getTargetPrimType :: PrimitiveType -> String
getTargetPrimType primType = case primType of
  PrimitiveType_void   -> error (show primType)
  PrimitiveType_bool   -> "Bool"
  PrimitiveType_byte   -> "Int"
  PrimitiveType_int    -> "Int"
  PrimitiveType_int64  -> "Integer"
  PrimitiveType_uint64 -> "Integer"
  PrimitiveType_float  -> error (show primType)
  PrimitiveType_double -> "Float"
  PrimitiveType_char   -> "Char"
  PrimitiveType_string -> "String"
