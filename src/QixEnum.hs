module QixEnum (
  makeEnumTypes,
  getAllEnumTypes
) where

import QixAbsSyntax
import Data.List(intersperse)

makeEnumTypes :: QixFile -> [[String]]
makeEnumTypes qixFile =
  let enumTypes = getAllEnumTypes qixFile
   in [ map makeEnumDataType enumTypes
      , map makeValueTypeInstance enumTypes
      ]

getAllEnumTypes :: QixFile -> [EnumDefinition]
getAllEnumTypes (QixFile _ defs) = [ enumDef | QixDef_Type (TypeDef_Enum enumDef) <- map snd defs ]

makeEnumDataType :: EnumDefinition -> String
makeEnumDataType (EnumDef enumId literals) =
  let lits = [lit | EnumLiteral_Def lit _ _ <- literals]
   in unwords $ ["data", enumId, "="] ++ (intersperse "|" lits) ++ ["deriving (Show, Read, Eq, Ord, Enum)"]

makeValueTypeInstance :: EnumDefinition -> String
makeValueTypeInstance (EnumDef id _) = unlines 
  [ "instance ValueType " ++ id ++ " where"
  , "  toValue lit = ValueString (show lit)"
  , "  fromValue v = case v of"
  , "    ValueString txt -> read txt"
  , "    _ -> error \"fromValue " ++ id ++ "\""
  ]
