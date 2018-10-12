module QixParser where

import System.IO (stdout, hPutStr, hFlush)
import QixAbsSyntax
import QixTokens
import Text.Parsec
--import Text.Parsec.Prim(unexpected)
import Text.Parsec.Pos
import Data.Char (toLower)
import Debug.Trace
import Data.Maybe(isJust)

-- qixFile = "qix.qdl"
qixFile = "qvnext.qdl"
allFiles = ["connectivity.qdl", "qix.qdl", "qrs.qdl", "qvnext.qdl", "search.qdl"]
type Parser a = Parsec [(SourcePos, QixToken)] () a

parseAll = mapM_ runParse allFiles >>= print


runParse file = do
  hPutStr stdout ("Analyzing " ++ file ++ "\t- ") >> hFlush stdout
  str <- readFile file
  hPutStr stdout ("FileSize: " ++ (show $ length str)) >> hFlush stdout
  let tokens = tokenize (initialPos qixFile) str
  putStrLn (", Token count: " ++ (show $ length tokens))
  return (parse parseQixFile "" tokens)

parseQixFile :: Parser QixFile
parseQixFile = do
  lib     <- optionMaybe parseLibrary
  qixDefs <- inCurly (many parseQixDefinition)
  return (QixFile lib qixDefs)

parseLibrary :: Parser Library
parseLibrary = parseKeyWord Key_library >> parseId

parseQixDefinition :: Parser ([Attribute], QixDefinition)
parseQixDefinition = do
  attribs <- optionalAttributes
  def     <-     fmap QixDef_Rule      parseRule
             <|> fmap QixDef_Class     (parseKeyWord Key_class     >> parseClass)
             <|> fmap QixDef_Extend    (parseKeyWord Key_extend    >> parseClass)
             <|> fmap QixDef_Interface (parseKeyWord Key_interface >> parseClass)
             <|> fmap QixDef_Type      parseType
             <|> fmap QixDef_Include   parseInclude
  parseSep ";"
  return (attribs, def)

parseInclude :: Parser Id
parseInclude = parseKeyWord Key_include >> parseId

parseRule :: Parser RuleDefinition
parseRule = do
  parseKeyWord Key_rule
  attribs <- optionalAttributes
  optional (parseOp "*")
  ids     <- parseId `endBy` (optional (parseOp "*"))
  return (RuleDefinition attribs ids)

parseClass :: Parser ClassDefinition
parseClass = do
  id       <- parseId
  mextends <- optionMaybe (parseSep ":" >> (parseId `sepBy` parseSep ","))
  mbody    <- optionMaybe (inCurly (parseMethodDefinition `endBy` (parseSep ";")))
  return (ClassDef id mextends mbody)

parseMethodDefinition :: Parser MethodDefinition
parseMethodDefinition = do
  attribs <- optionalAttributes
  typeRef <- parseTypeRef
  id      <- parseId
  args    <- paren '(' ')' (parseMethodArgumentDefinition `sepBy` (parseSep ","))
  mconst  <- optionMaybe (parseKeyWord Key_const)
  return (MethodDef id attribs typeRef args (isJust mconst))

parseMethodArgumentDefinition :: Parser MethodArgumentDefinition
parseMethodArgumentDefinition = do
  attribs <- optionalAttributes
  typeRef <- parseTypeRef
  id      <- parseId <|> (parseKeyWord Key_String >> return "String")
  return (MethodArgumentDef id attribs typeRef)

parseType :: Parser TypeDefinition
parseType =
  parseKeyWord Key_typedef >>
    (     fmap TypeDef_Enum   parseEnum
      <|> fmap TypeDef_Struct parseStruct
      <|> fmap TypeDef_Alias  parseAlias
    )

parseEnum :: Parser EnumDefinition
parseEnum = do
  parseKeyWord Key_enum
  literals <- inCurly (parseEnumLiteral `endBy` (optional (parseSep ",")))
  id       <- parseId
  return (EnumDef id literals)

parseEnumLiteral :: Parser EnumLiteralDefinition
parseEnumLiteral = do
  attribs <- optionalAttributes
  id      <- parseId
  mint    <- optionMaybe (parseOp "=" >> parseValue)
  return (EnumLiteral_Def id attribs mint)

parseStruct :: Parser StructDefinition
parseStruct = do
  parseKeyWord Key_struct
  optional parseId
  fields <- inCurly (many parseStructMemberDefinition)
  id     <- parseId
  return (StructDef id fields)

parseStructMemberDefinition :: Parser StructMemberDefinition
parseStructMemberDefinition = do
  attribs <- optionalAttributes
  typeRef <- parseTypeRef
  id      <- parseId <|> (parseKeyWord Key_String >> return "String")
  val     <- optionMaybe (parseOp "=" >> parseValue)
  parseSep ";"
  return (StructMemberDef id attribs typeRef val)

parseAlias :: Parser TypeAliasDefinition
parseAlias = do
  typeRef <- parseTypeRef
  id      <- parseId
  return (TypeAliasDef id typeRef)

parseTypeRef :: Parser TypeRef
parseTypeRef = do
  mkey <- optionMaybe parseAnyKey
  case mkey of
    Nothing            -> fmap TypeRef_Name  parseId
    Just Key_REF       -> fmap TypeRef_Ref   (paren '(' ')' parseTypeRef)
    Just Key_SAFEARRAY -> fmap TypeRef_Array (paren '(' ')' parseTypeRef)
    Just keyWord -> if keyWord `elem` [Key_void .. Key_String]
                    then return (TypeRef_Primitive (keywordToPrimitiveType keyWord))
                    else unexpected ("Keyword " ++ show keyWord)

keywordToPrimitiveType :: KeyWord -> PrimitiveType
keywordToPrimitiveType key = read $ "PrimitiveType_" ++ (map toLower $ drop 4 $ show key)

optionalAttributes :: Parser [Attribute]
optionalAttributes = optionList '[' ']' "," parseAttribute

parseAttribute :: Parser Attribute
parseAttribute = do
  id     <- parseId
  isPlus <- fmap (maybe True (\_ -> False)) (optionMaybe (parseOp "-"))
  args   <- optionList '(' ')' "," parseAttributeArgument
  return (Attribute id isPlus args)

parseAttributeArgument :: Parser AttributeArgument
parseAttributeArgument =
      parseAttributeArgumentExprA
  <|> parseAttributeArgumentExprB
  <|> parseAttributeArgumentValue
  <|> parseAttributeArgumentClass

parseAttributeArgumentExprA :: Parser AttributeArgument
parseAttributeArgumentExprA = do
  id0 <- parseId
  op  <- optionMaybe parseAnyOp
  v   <- optionMaybe parseValue
  return (AttributeArgument_Expr (Just id0) op v)

parseAttributeArgumentExprB :: Parser AttributeArgument
parseAttributeArgumentExprB = do
  op  <- parseOp ">" <|> parseOp ">="
  v <- parseValue
  return (AttributeArgument_Expr Nothing (Just op) (Just v))

parseAttributeArgumentValue :: Parser AttributeArgument
parseAttributeArgumentValue = fmap AttributeArgument_Val parseValue

parseAttributeArgumentClass :: Parser AttributeArgument
parseAttributeArgumentClass = parseKeyWord Key_class >> fmap AttributeArgument_Class parseId

parseValue :: Parser Value
parseValue = 
      parseNumValue
  <|> (fmap Value_Enum parseId)
  <|> parseString

parseNumValue :: Parser Value
parseNumValue = do
  n <- (parseNum <|> parseHex)
  maybeRange <- optionMaybe (parseSep "..")
  case maybeRange of
    Nothing -> return n
    Just _  -> fmap (Value_Range n) (parseNum <|> parseHex)

parseNum :: Parser Value
parseNum = do
  n      <- parseDigits
  mSep   <- optionMaybe (parseSep ".")
  mFrac  <- maybe (return Nothing) (\_ -> optionMaybe parseDigits) mSep
  case mFrac of
    Nothing -> return (Value_Int (read n))
    Just frac -> return (Value_Float (read (n ++ "." ++ frac)))
  
--- Utils
optionList :: Char -> Char -> String -> Parser a -> Parser [a]
optionList p0 p1 sep p = do
  m <- optionMaybe (paren p0 p1 (p `sepBy` (parseSep sep)))
  return (maybe [] id m)

paren :: Char -> Char -> Parser a -> Parser a
paren c0 c1 p = between (parsePar c0) (parsePar c1) p

inCurly :: Parser a -> Parser a
inCurly = paren '{' '}' 

parseDigits :: Parser String
parseDigits = do
  mNeg <- optionMaybe (parseOp "-")
  parseToken $ \(_,t) ->
    case t of
      Token_Digits digits -> Just (if isJust mNeg then '-':digits else digits)
      _ -> Nothing

parseAnyOp :: Parser String
parseAnyOp = parseToken tokenAnyOp

parseOp :: String -> Parser String
parseOp c0 = parseToken $ \(_,t) ->
  case t of
    Token_Op c1 -> if (c0 == c1) then Just c0 else Nothing
    _ -> Nothing

parseString :: Parser Value
parseString = parseToken $ \(_,t) ->
  case t of
    Token_String str -> Just (Value_String str)
    _ -> Nothing

parseHex :: Parser Value
parseHex = parseToken $ \(_,t) ->
  case t of
    Token_Hex str -> Just (Value_Hex str)
    _ -> Nothing

parseSep :: String -> Parser String
parseSep c0 = parseToken $ \(_,t) ->
  case t of
    Token_Sep c1 -> if (c0 == c1) then Just c0 else Nothing
    _ -> Nothing

parsePar :: Char -> Parser Char
parsePar c0 = parseToken $ \(_,t) ->
  case t of
    Token_Par c1 -> if (c0 == c1) then Just c0 else Nothing
    _ -> Nothing

parseAnyKey :: Parser KeyWord
parseAnyKey = parseToken tokenAnyKey

parseKeyWord :: KeyWord -> Parser KeyWord
parseKeyWord k = parseToken (tokenKey k)

--parseToken :: Parser a
parseToken = token show fst

parseId :: Parser String
parseId = parseToken tokenId

tokenId :: (SourcePos, QixToken) -> Maybe String
tokenId (_,Token_Id s) = Just s
tokenId _ = Nothing

tokenAnyOp :: (SourcePos, QixToken) -> Maybe String
tokenAnyOp (_,Token_Op token) = Just token
tokenAnyOp _ = Nothing

tokenAnyKey :: (SourcePos, QixToken) -> Maybe KeyWord
tokenAnyKey (_,Token_Key token) = Just token
tokenAnyKey _ = Nothing

tokenKey :: KeyWord -> (SourcePos, QixToken) -> Maybe KeyWord
tokenKey expected token = do key <- tokenAnyKey token
                             (if key == expected then Just key else Nothing)
