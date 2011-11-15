
module Lexer (Token(..), tokenize) where

import Data.Char
import Data.Maybe
import Text.ParserCombinators.Parsec

data Token =
    TIdent String
  | TKeyword String
  | TInt Integer
  | TFloat Double
  | TChar Char
  | TString String
  | TOper String
  | TDot
  | TComma
  | TArrow
  | TEquals
  | TEnd
  | TLParen
  | TRParen
  | TLBracket
  | TRBracket
  | TColon
  | TAt
  | TLBrace
  | TRBrace
  deriving (Eq, Show)

tokenize :: SourceName -> String -> Either ParseError [(Token, SourcePos, Maybe String)]
tokenize = parse file

ignorables = many $ (space >> return ()) <|> (comment >> return ())

file :: Parser [(Token, SourcePos, Maybe String)]
file = do
  ignorables
  xs <- many ptoken
  eof
  return xs

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

bool :: Parser a -> Parser Bool
bool = option False . (>> return True)

lineComment = do
  string "//"
  d  <- bool $ char '/'
  xs <- many $ noneOf "\n"
  (char '\n' >> return ()) <|> eof
  return $ trim xs

blockComment = do
  string "/*"
  d <- bool $ char '*'
  xs <- blockCommentBody
  string "*/"
  return $ trim xs

blockCommentBody = do
  xs     <- many $ noneOf "/*"
  atEnd  <- bool $ lookAhead $ string "*/"
  atNest <- bool $ lookAhead $ string "/*"
  case (atEnd, atNest) of
    (False, False) -> do
      y  <- anyChar
      ys <- blockCommentBody
      return $ xs ++ y : ys
    (True, False)  -> do
      return xs
    (False, True)  -> do
      ys <- blockComment
      zs <- blockCommentBody
      return $ xs ++ ys ++ zs
    (True, True)   -> fail "The impossible happened in blockCommentBody."

comment = lineComment <|> blockComment

ptoken :: Parser (Token, SourcePos, Maybe String)
ptoken = do
  p <- getPosition
  t <- puncEquals <|> punc <|> ident <|> number <|> qstring <|> character <|> operator
  spaces
  c <- optionMaybe comment
  ignorables
  return (t, p, c)

punc :: Parser Token
punc = foldr1 (<|>) $ map (\(xs, tok) -> string xs >> return tok) puncs

puncEquals :: Parser Token
puncEquals = do
  char '='
  isArrow <- option False $ char '>' >> return True
  return $ if isArrow then TArrow else TEquals

puncs =
  [ (".", TDot)
  , (",", TComma)
  , (";", TEnd)
  , ("(", TLParen)
  , (")", TRParen)
  , ("{", TLBrace)
  , ("}", TRBrace)
  , (":", TColon)
  , ("@", TAt)
  , ("[", TLBracket)
  , ("]", TRBracket)
  ]

keywords =
  [ "_"
  , "with"
  , "if"
  , "elif"
  , "else"
  , "for"
  , "in"
  , "while"
  , "next"
  , "done"
  , "return"
  , "do"
  , "decide"
  , "def"
  , "ns"
  , "export"
  , "global"
  , "import"
  , "struct"
  , "union"
  , "enum"
  , "mask"
  , "from"
  , "closed"
  , "alias"
  , "first"
  , "last"
  , "iteration"
  , "iterations"
  ]

ident :: Parser Token
ident = do
  x  <- lu
  xs <- many $ lu <|> digit
  let xs' = x : xs
  return $ case xs' `elem` keywords of
    True  -> TKeyword xs'
    False -> TIdent xs'
  where
    lu = letter <|> char '_'

number :: Parser Token
number = digitStartNumber <|> dotStartNumber

dotStartNumber :: Parser Token
dotStartNumber = do
  char '.'
  fracStr <- many1 digit
  exp     <- option 1 expPart
  let frac = read ('.' : fracStr) :: Double
  return $ TFloat $ frac ** fromInteger exp

digitStartNumber :: Parser Token
digitStartNumber = do
  intStr  <- many1 digit
  fracStr <- optionMaybe $ char '.' >> many digit
  exp     <- option 1 expPart
  let int  = read intStr :: Integer
  let frac = fmap (read . ('.' :)) fracStr :: Maybe Double
  case frac of
    Nothing   -> return $ TInt $ int ^ exp
    Just frac -> return $ TFloat $ (fromInteger int + frac) ** fromInteger exp

expPart :: Parser Integer
expPart = do
  oneOf ['e', 'E']
  sign   <- option '+' $ oneOf "+-"
  intStr <- many1 digit
  let int = read intStr
  return $ if sign == '+' then int else -int

{-
 -
COMMENT
    :   '//' ~('\n'|'\r')* '\r'? '\n' {$channel=HIDDEN;}
    |   '/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
    ;
 -}

qstring :: Parser Token
qstring = do
  char '"'
  xs <- many $ escape <|> noneOf "\\\""
  char '"'
  return $ TString xs

escape :: Parser Char
escape = normal_escape <|> unicode_escape

normal_escapes :: [(Char, Char)]
normal_escapes =
  [ ('b', '\b')
  , ('t', '\t')
  , ('n', '\n')
  , ('f', '\f')
  , ('r', '\r')
  , ('\'', '\'')
  , ('"', '"')
  , ('\\', '\\')
  ]

normal_escape :: Parser Char
normal_escape = do
  char '\\'
  x <- oneOf "btnfr'\"\\"
  return $ fromJust $ lookup x normal_escapes

character :: Parser Token
character = do
  char '\''
  x <- escape <|> noneOf "'\\"
  char '\''
  return $ TChar x

unicode_escape :: Parser Char
unicode_escape = do
  char '\\'
  char 'u'
  a <- hexDigit
  b <- hexDigit
  c <- hexDigit
  d <- hexDigit
  return $ chr $
    (((((hex_to_int a * 16) + hex_to_int b) * 16) + hex_to_int c) * 16) + hex_to_int d

hex_to_int :: Char -> Int
hex_to_int '0' = 0
hex_to_int '1' = 1
hex_to_int '2' = 2
hex_to_int '3' = 3
hex_to_int '4' = 4
hex_to_int '5' = 5
hex_to_int '6' = 6
hex_to_int '7' = 7
hex_to_int '8' = 8
hex_to_int '9' = 9
hex_to_int 'a' = 10
hex_to_int 'b' = 11
hex_to_int 'c' = 12
hex_to_int 'd' = 13
hex_to_int 'e' = 14
hex_to_int 'f' = 15
hex_to_int 'A' = 10
hex_to_int 'B' = 11
hex_to_int 'C' = 12
hex_to_int 'D' = 13
hex_to_int 'E' = 14
hex_to_int 'F' = 15

operator :: Parser Token
operator = do
  xs <- many1 $ oneOf "~!$%^&*-+=|/<>?"
  return $ TOper xs

