
{- TODO change lexer to recognize keywords separately -}
{- TODO add loop labels -}
{- TODO add operator importing -}
{- TODO add qualified operator references -}
{- TODO add some composition operators -}
{- TODO add literal arrays and struct-likes -}

module Parser where

import Data.Either
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim

import Lexer

parseFile :: SourceName -> String -> Either ParseError Namespace
parseFile name text = either Left (parse file name) $ tokenize name text

type P = Parsec [(Token, SourcePos)] ()

data SimpleName = SimpleName String deriving (Eq, Show)

data Name = Name [SimpleName] deriving (Eq, Show)

data Param =
    TypeParam SimpleName
  | ValueParam SimpleName Type
  deriving (Eq, Show)

data Kind = Struct | Union | Enum | Mask deriving (Eq, Show)

data TypeDef = TypeDef Kind [Expr] SimpleName [Param] [StructElem] deriving (Eq, Show)

data StructElem = StructElem SimpleName (Maybe Type) (Maybe Expr) deriving (Eq, Show)

type Type = Expr

data Expr =
    EApp Expr [Expr]
  | EInt Integer
  | EFloat Double
  | EChar Char
  | EString String
  | EName Name
  | ENew Name [Expr]
  | EFnValue [Param] Type FnBody
  | EFnType [Param] Type
  deriving (Eq, Show)

type FnBody = Either Block Expr

type Block = [Stmt]

data Stmt =
    SExpr Expr
  | SDef SimpleName Expr
  | SIf (Maybe Expr) Expr Block [(Expr, Block)] (Maybe Block)
  | SWhile Expr Block (Maybe Block)
  | SFor SimpleName Type Expr Block (Maybe Block)
  | SNext
  | SDone
  | SReturn (Maybe Expr)
  | SDecide [Expr] [([Maybe Expr], Block)]
  | SDo Block
  | SEmpty
  deriving (Eq, Show)

data Namespace = Namespace Name [Import] [NsDef] deriving (Eq, Show)

data NsDef = NsDef Bool Bool SimpleName Expr deriving (Eq, Show)

data Import = Import Name (Maybe [SimpleName]) deriving (Eq, Show)

tok :: (Token -> Maybe a) -> P a
tok p = token (show . fst) snd (p . fst)

isTok :: Token -> P ()
isTok t = tok $ \t' -> if t == t' then Just () else Nothing

simpleName :: P SimpleName
simpleName =
  tok $ \t -> case t of
    TIdent xs -> Just $ SimpleName xs
    _ -> Nothing

end, dot, comma, lParen, rParen, lBracket, rBracket, colon, equals, arrow :: P ()
end = isTok TEnd
dot = isTok TDot
comma = isTok TComma
lParen = isTok TLParen
rParen = isTok TRParen
lBracket = isTok TLBracket
rBracket = isTok TRBracket
colon = isTok TColon
equals = isTok TEquals
arrow = isTok TArrow

name :: P Name
name = sepBy1 simpleName dot >>= return . Name

delimit :: P start -> P end -> P a -> P a
delimit start end body = do
  start
  x <- body
  end
  return x

parens :: P a -> P a
parens = delimit lParen rParen

brackets :: P a -> P a
brackets = delimit lBracket rBracket

params :: P [Param]
params = parens $ sepEndBy param comma

param :: P Param
param = do
  n <- simpleName
  t <- optionMaybe $ colon >> expr
  return $ case t of
    Just t -> ValueParam n t
    Nothing -> TypeParam n

{-
	The arguments may be given for enum or mask to specify the backing type.
	In a struct or union, all elements must have a type; in an enum or mask, none must.
	In a struct, all elements must have an initial value; in a union, exactly one must.
	In an enum or mask, the parameters must all be static.
  In an enum or mask, any element may have an initial value, but the initial
  values must all be valid.
-}
typeDef :: P TypeDef
typeDef = do
  k  <- kind
  as <- option [] arguments
  n  <- simpleName
  ps <- option [] params
  es <- brackets $ sepEndBy structElem comma
  end
  return $ TypeDef k as n ps es

isKw :: String -> P ()
isKw = isTok . TKeyword

kind :: P Kind
kind =
  (isKw "struct" >> return Struct)
  <|>
  (isKw "union"  >> return Union)
  <|>
  (isKw "enum"   >> return Enum)
  <|>
  (isKw "mask"   >> return Mask)

structElem :: P StructElem
structElem = do
  n <- simpleName
  t <- optionMaybe $ colon >> expr
  v <- optionMaybe $ equals >> expr
  return $ StructElem n t v

arguments :: P [Expr]
arguments = parens $ sepEndBy expr comma

exprHead :: P Expr
exprHead =
  literal
  <|>
{-
  Hard-wird names:

	Applied to values:
  length (get length of array)
  tag (get tag value of union)
  ref (increment reference count)
  unref (decrement reference count)
  const (safe cast from ptr to const)
  cast (unsafe cast to any specified type)
  first, last, iteration (get information about loop iterations)
  
  Applied to types:
  ptr (pointer, also used for arrays)
  const (like ptr, but read-only)
  tag (get tag type of union)
  and all the primitive types
-}
  (name >>= return . EName)
  <|>
  unaryOpExpr
  <|>
  parens expr

data Prec =
    PCompose
  | PCompare
  | PLogicOr
  | PLogicAnd
  | PUser
  | PAdd
  | PMul
  | PShift
  | PBitwiseOr
  | PBitwiseAnd
  | PExp
  deriving (Bounded, Enum, Eq, Ord, Show)

precedence =
  [ ("<=>", PCompare)
  , ("==",  PCompare)
  , ("!=",  PCompare)
  , ("<=",  PCompare)
  , (">=",  PCompare)
  , (">",   PCompare)
  , ("<",   PCompare)
  , ("||",  PLogicOr)
  , ("&&",  PLogicAnd)
  , ("+",   PAdd)
  , ("-",   PAdd)
  , ("*",   PMul)
  , ("/",   PMul)
  , ("%",   PMul)
  , (">>",  PShift)
  , ("<<",  PShift)
  , ("|",   PBitwiseOr)
  , ("&",   PBitwiseAnd)
  , ("^",   PExp)
  ]

oper :: P (Prec, Expr)
oper = do
  o <- tok $ \t -> case t of
    TOper xs -> Just xs
    _ -> Nothing
  let p = maybe PUser id $ lookup o precedence
  return (p, EName $ Name [SimpleName o])

unaryOpExpr :: P Expr
unaryOpExpr = do
  (_, o) <- oper
  e <- expr
  return $ EApp o [e]

type ExprTail = Either [Expr] (Prec, Expr, Expr)

exprTail :: P ExprTail
exprTail =
  (arguments >>= return . Left)
  <|>
  do
    (p, o) <- oper
    e <- expr
    return $ Right (p, o, e)

expr :: P Expr
expr = do
  h  <- exprHead
  ts <- many exprTail
  let (h', ts') = squashApps h ts
  return $ applyPrec h' ts'

squashApps :: Expr -> [ExprTail] -> (Expr, [(Prec, Expr, Expr)])
squashApps h [] = (h, [])
squashApps h (Left as : ts) = squashApps (EApp h as) ts
squashApps h (Right (p, o, e) : ts) = (h, (p, o, e') : ts')
  where
    (e', ts') = squashApps e ts

fst3 (x, _, _) = x

snd3 (_, x, _) = x

thd3 (_, _, x) = x

applyPrec :: Expr -> [(Prec, Expr, Expr)] -> Expr
applyPrec h [] = h
applyPrec h ts = EApp o [left', right']
  where
    minPrec = minimum $ map fst3 ts
    (left, ((_, o, e) : rTail)) = span ((< minPrec) . fst3) ts
    left' = applyPrec h left
    right' = applyPrec e rTail

literal :: P Expr
literal = tokLiteral <|> new <|> fn

tokLiteral :: P Expr
tokLiteral = tok $ \t -> case t of
  TInt n     -> Just $ EInt n
  TFloat n   -> Just $ EFloat n
  TChar c    -> Just $ EChar c
  TString xs -> Just $ EString xs
  _          -> Nothing

fn :: P Expr
fn = do
  ps <- params
  t  <- fnValueTail <|> fnTypeTail
  return $ t ps

fnValueTail, fnTypeTail :: P ([Param] -> Expr)

fnValueTail = do
  colon
  t <- expr
  arrow
  b <- fnBody
  return $ \ps -> EFnValue ps t b

fnBody = (expr >>= return . Right) <|> (block >>= return . Left)

fnTypeTail = do
  arrow
  t <- expr
  return $ \ps -> EFnType ps t

new :: P Expr
new = do
  isKw "new"
  n  <- name
  as <- arguments
  return $ ENew n as

block :: P Block
block = brackets $ many stmt

stmt, def, cond, while, for, ret, decide, exec :: P Stmt
stmt =
  def <|> cond <|> while <|> for <|> ret <|> decide <|> exec
  <|>
  (isKw "next" >> end >> return SNext)
  <|>
  (isKw "done" >> end >> return SDone)
  <|>
  (end >> return SEmpty)
  <|>
  do
    e <- expr
    end
    return $ SExpr e

ret = do
  isKw "return"
  e <- optionMaybe expr
  end
  return $ SReturn e

exec = do
  isKw "do"
  b <- block
  return $ SDo b

while = do
  isKw "while"
  e <- expr
  b <- block
  n <- next
  return $ SWhile e b n

{-
  The for-each loop expression must be of a type for which all the necessary
  functions are defined to allow it to be used as an iterable thing.
-}
for = do
  isKw "for"
  lParen
  v <- simpleName
  colon
  t <- expr
  isKw "in"
  e <- expr
  rParen
  b <- block
  n <- next
  return $ SFor v t e b n

next :: P (Maybe Block)
next = optionMaybe $ isKw "next" >> block

{-
  The initial "with" expression is a predicate function applied to each test
  expression. By default, the identity function over Booleans is used.
-}

cond = do
  w <- optionMaybe $ isKw "with" >> expr
  isKw "if"
  t <- expr
  b <- block
  elifs <- many $ do
    isKw "elif"
    e <- expr
    b <- block
    return (e, b)
  e <- optionMaybe $ isKw "else" >> block
  return $ SIf w t b elifs e

def = do
  isKw "def"
  n <- simpleName
  e <- defTail
  return $ SDef n e

defTail =
  do
    equals
    e <- expr
    end
    return e
  <|>
  do
    ps <- params
    colon
    t  <- expr
    equals
    b  <- fnBody
    return $ EFnValue ps t b

fnBodyWithEnd =
  do
    e <- expr
    end
    return $ Right e
  <|>
  (block >>= return . Left)

decide = do
  isKw "decide"
  as <- arguments
  os <- brackets $ many decision
  return $ SDecide as os

exprOrBlock =
  block
  <|>
  do
    e <- expr
    end
    return [SExpr e]

decision = do
  es <- parens $ sepEndBy decisionExpr comma
  arrow
  b  <- exprOrBlock
  return (es, b)

decisionExpr =
  (isKw "_" >> return Nothing)
  <|>
  (expr >>= return . Just)

file :: P Namespace
file = do
  n  <- option (Name [SimpleName "Main"]) $ do
    isKw "ns"
    n <- name
    end
    return n
  es <- many nsElem
  eof
  let (is, ds) = partitionEithers es
  return $ Namespace n is ds

parseEither :: P a -> P b -> P (Either a b)
parseEither a b = (a >>= return . Left) <|> (b >>= return . Right)

nsElem :: P (Either Import NsDef)
nsElem = parseEither imp nsDef

imp :: P Import
imp = do
  isKw "import"
  ns <- sepEndBy simpleName comma
  isKw "from"
  n  <- name
  end
  return $ Import n $ case ns of
    [] -> Nothing
    ns -> Just ns

isKw' :: String -> P String
isKw' xs = tok $ \t -> case t of
  TKeyword ys -> if xs == ys then Just xs else Nothing
  _         -> Nothing

nsDef :: P NsDef
nsDef = do
  ms <- many $ isKw' "export" <|> isKw' "global"
  let e = "export" `elem` ms
  let g = "global" `elem` ms
  SDef n v <- def
  return $ NsDef e g n v

