
module Lexer where

import Text.ParserCombinators.Parsec

data Token =
    TIdent String
  | TInt Integer
  | TFloat Double
  | TChar Char
  | TOper String

token = do
  spaces
  ident <|> number <|> string <|> character <|> operator

ident = do
  x  <- lu
  xs <- many $ lu <|> digit
  return $ TIdent $ x : xs
  where
    lu = letter <|> char '_'

dotStartNumber = do
  char '.'
  fracStr <- many1 digit
  let frac = read ('.' : fracStr) :: Double
  return $ TFloat frac

digitStartNumber = do
  intStr  <- many1 digit
  fracStr <- optionMaybe $ char '.' >> many digit
  exp     <- option 1 exponent
  let int  = read intStr :: Integer
  let frac = fmap read ('.' : fracStr) :: Maybe Double
  case frac of
    Nothing   -> return $ TInt $ int ^ exp
    Just frac -> return $ TFloat $ (fromInteger int + frac) ** exp

exponent = do
  oneOf ['e', 'E']
  sign   <- option '+' $ oneOf "+-"
  intStr <- many1 digit
  let int = read intStr :: Integer
  return $ if sign == '+' then int else -int

COMMENT
    :   '//' ~('\n'|'\r')* '\r'? '\n' {$channel=HIDDEN;}
    |   '/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
    ;

string = do
  char '"'
  xs <- many $ escape <|> noneOf "\\\""
  char '"'
  return $ TString xs

escape = normal_escape <|> unicode_escape

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

normal_escape = do
  char '\\'
  x <- oneOf "btnfr'\"\\"
  return $ fromJust $ lookup x normal_escapes

character = do
  char '\''
  x <- escape <|> noneOf "'\\"
  char '\''
  return $ TChar x

unicode_escape = do
  char '\\'
  char 'u'
  a <- hexDigit
  b <- hexDigit
  c <- hexDigit
  d <- hexDigit
  return $ chr $
    (((((hex_to_int a * 16) + hex_to_int b) * 16) + hex_to_int c) * 16) + hex_to_int d

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

operator = do
  xs <- many1 $ oneOf "~!@$%^&*-+=|/<>?"
  return $ TOper xs

