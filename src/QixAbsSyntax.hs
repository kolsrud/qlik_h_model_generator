module QixAbsSyntax where

import Text.PrettyPrint

data QixFile = QixFile (Maybe Library) [([Attribute], QixDefinition)]
               deriving (Show)

data QixDefinition = QixDef_Rule      RuleDefinition
                   | QixDef_Class     ClassDefinition
                   | QixDef_Extend    ClassDefinition
                   | QixDef_Interface ClassDefinition
                   | QixDef_Type      TypeDefinition
                   | QixDef_Include   Id
                     deriving (Show)

data TypeDefinition = TypeDef_Enum   EnumDefinition
                    | TypeDef_Struct StructDefinition
                    | TypeDef_Alias  TypeAliasDefinition
               deriving (Show)

data TypeAliasDefinition = TypeAliasDef Id TypeRef
               deriving (Show)
data RuleDefinition = RuleDefinition [Attribute] [Id]
               deriving (Show)
data EnumDefinition = EnumDef Id [EnumLiteralDefinition]
               deriving (Show)
data EnumLiteralDefinition = EnumLiteral_Def Id [Attribute] (Maybe Value)
               deriving (Show)

data StructDefinition = StructDef Id [StructMemberDefinition]
               deriving (Show)
data StructMemberDefinition = StructMemberDef Id [Attribute] TypeRef (Maybe Value)
               deriving (Show)
data Value = Value_Int    Int
           | Value_Float  Float
           | Value_Hex    String
           | Value_Enum   String
           | Value_String String
           | Value_Range  Value Value
               deriving (Show)

data TypeRef = TypeRef_Primitive PrimitiveType
             | TypeRef_Array TypeRef
             | TypeRef_Ref TypeRef
             | TypeRef_Name Id
               deriving (Show)

type Library = String
type Id = String

data PrimitiveType = PrimitiveType_void
                   | PrimitiveType_bool
                   | PrimitiveType_byte
                   | PrimitiveType_int
                   | PrimitiveType_int64
                   | PrimitiveType_uint64
                   | PrimitiveType_float
                   | PrimitiveType_double
                   | PrimitiveType_char
                   | PrimitiveType_string
                     deriving (Show,Read)

data Attribute = Attribute Id Bool [AttributeArgument]
               deriving (Show)
data AttributeArgument = AttributeArgument_Expr (Maybe Id) (Maybe String) (Maybe Value)
                       | AttributeArgument_Val Value
                       | AttributeArgument_Class Id
               deriving (Show)

data ClassDefinition = ClassDef Id (Maybe [Id]) (Maybe [MethodDefinition])
               deriving (Show)
data MethodDefinition = MethodDef Id [Attribute] TypeRef [MethodArgumentDefinition] Bool -- Bool = is const
               deriving (Show)
data MethodArgumentDefinition = MethodArgumentDef Id [Attribute] TypeRef
               deriving (Show)

printQix :: QixFile -> String
printQix qix = render (printQixFile qix)

printQixFile :: QixFile -> Doc
printQixFile (QixFile m_lib defs) =
     maybe empty text m_lib
  $+$ text "{"
  $+$ (nest 2 $ vcat (map printQixDef defs))
  $+$ text "}"

printQixDef :: ([Attribute], QixDefinition) -> Doc
printQixDef _ = text "<qixdef>"

