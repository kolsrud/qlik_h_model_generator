module QixTokens where

import Data.Char
import Text.Parsec.Pos
data QixToken = Token_Par     Char
              | Token_Sep     String
              | Token_Key     KeyWord
              | Token_Op      String
              | Token_Digits  String
              | Token_Hex     String
              | Token_String  String
              | Token_Id      String
              | Token_Comment String
                deriving Show

data KeyWord = Key_library
             | Key_include
             | Key_enum
             | Key_rule
             | Key_class
             | Key_extend
             | Key_interface
             | Key_const
             | Key_struct
             | Key_typedef
             | Key_void
             | Key_bool
             | Key_char
             | Key_byte
             | Key_int
             | Key_int64
             | Key_uint64
             | Key_double
             | Key_String
             | Key_REF
             | Key_SAFEARRAY
               deriving (Show, Enum, Eq)

testStr = "library QlikView\n\
\{\n\
\\ttypedef enum {\n\
\\t\tLOCMSG_SCRIPTEDITOR_EMPTY_MESSAGE = 0,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_SAVING_STARTED = 1,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_BYTES_LEFT = 2,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_STORING_TABLES = 3,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_QVD_ROWS_SO_FAR = 4,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_CONNECTED = 5,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_CONNECTING_TO = 6,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_CONNECT_FAILED = 7,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_QVD_ROWISH = 8,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_QVD_COLUMNAR = 9,\n\
\\t\tLOCMSG_SCRIPTEDITOR_ERROR = 10,\n\
\\t\tLOCMSG_SCRIPTEDITOR_DONE = 11,\n\
\\t\tLOCMSG_SCRIPTEDITOR_LOAD_EXTERNAL_DATA = 12,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_OLD_QVD_ISLOADING = 13,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_QVC_LOADING = 14,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_QVD_BUFFERED = 15,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_QVC_PREPARING = 16,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PROGRESS_QVC_APPENDING = 17,\n\
\\t\tLOCMSG_SCRIPTEDITOR_REMOVE_SYNTHETIC = 18,\n\
\\t\tLOCMSG_SCRIPTEDITOR_PENDING_LINKEDTABLE_FETCHING = 19,\n\
\\t\tLOCMSG_SCRIPTEDITOR_RELOAD = 20,\n\
\\t\tLOCMSG_SCRIPTEDITOR_LINES_FETCHED = 21,\n\
\    \n\
\\t} NxLocalizedMessageCode;\n"

testQix = do str <- readFile "qix.qdl"
             putStrLn (show $ length str)
             putStrLn (show $ length (tokenize (initialPos "MockFile") str))

keywords = [ (drop 4 (show key), key) | key <- enumFrom Key_library ]

tokenize :: SourcePos -> String -> [(SourcePos, QixToken)]
tokenize _ [] = []
tokenize p ('\n':cs) = tokenize (newline p) cs
tokenize p ('/':'/':cs) = tokenize (newline p) (dropRestOfLine cs)
tokenize p ('0':'x':cs) = tokenizeHex cs
 where
  isHexDigit c = isDigit c || (c `elem` ['A'..'F'])

  tokenizeHex css =
    let (str, rest) = span isHexDigit css
     in (p, Token_Hex str):
        (tokenize (inc ((length str)+2) p) rest)

tokenize pos css@(c:cs)
  | c == '\t'  = tokenize (inc 8 pos) cs
  | isSpace c  = tokenize (inc 1 pos) cs
  | isSep c    = tokenizeSep css
  | isPar c    = (pos, Token_Par c):(tokenize (inc 1 pos) cs)
  | isOp c     = tokenizeOp css
  | c == '"'   = tokenizeString cs
  | isDigit c  = tokenizeDigits css
  | isIdChar c = tokenizeId css
  | take 3 css == "/**" = dropComment (inc 3 pos) (drop 3 css)
  | otherwise  = error (take 100 css)
 where
  isOp  = (flip elem) "=->*"
  isPar = (flip elem) "()[]{}"
  isSep = (flip elem) ".;:,"

  tokenizeSeries :: (Char -> Bool) -> (String -> QixToken) -> String -> [(SourcePos, QixToken)]
  tokenizeSeries f constructor css =
    let (chars, rest) = span f css
     in (pos, constructor chars):
        (tokenize (inc (length chars) pos) rest)

  tokenizeOp (c0:c1:rest)
    | [c0,c1] `elem` [">="] = (pos, Token_Op [c0,c1]):(tokenize (inc 2 pos) rest)
    | otherwise = (pos, Token_Op [c0]):(tokenize (inc 1 pos) (c1:rest))

  tokenizeSep    css = tokenizeSeries isSep        Token_Sep    css
  tokenizeDigits css = tokenizeSeries isDigit      Token_Digits css
  tokenizeId     css = tokenizeSeries isIdBodyChar interpretId  css

  tokenizeString css =
    let (str, rest) = break (=='"') css
     in (pos, Token_String str):
        (tokenize (inc (length str) pos) (tail rest))

  isIdChar c = isAlpha c || elem c "_"
  isIdBodyChar c = isIdChar c || isDigit c
  interpretId str =
    maybe (Token_Id str) Token_Key (lookup str keywords)

  dropComment pos css = case css of
    ('\t':str)    -> dropComment (inc 8 pos) str
    ('\n':str)    -> dropComment (newline pos) str
    ('*':'/':str) -> tokenize (inc 2 pos) str
    (_:str)       -> dropComment (inc 1 pos) str
    ""            -> []
 
inc = flip incSourceColumn

newline :: SourcePos -> SourcePos
newline p = incSourceLine (setSourceColumn p 0) 1
  
dropRestOfLine :: String -> String
dropRestOfLine s = let rest = dropWhile (/='\n') s
                    in if null rest
                       then ""
                       else tail rest
