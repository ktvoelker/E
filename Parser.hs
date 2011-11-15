
{-# LANGUAGE TupleSections #-}
module Parser
  ( module AST
  , parseFile
  ) where

import Control.Monad
import Data.Either
import Text.Parsec
import Text.Parsec.Prim

import AST
import Lexer

parseFile :: SourceName -> String -> Either ParseError Namespace
parseFile name text = either Left (parse file name) $ tokenize name text

tok :: (Token -> Maybe a) -> P a
tok = fmap fst . tokWithComment

tokWithComment :: (Token -> Maybe a) -> P (a, Maybe String)
tokWithComment p = token (show . fst3) snd3 (\t -> fmap (, thd3 t) $ p $ fst3 t)

isTok :: Token -> P ()
isTok t = tok $ \t' -> if t == t' then Just () else Nothing

simpleName :: P SimpleName
simpleName =
  tok $ \t -> case t of
    TIdent xs -> Just $ SimpleName xs
    _ -> Nothing

end, dot, comma, lParen, rParen, lBrace, rBrace, colon, equals, arrow :: P ()
end = isTok TEnd
dot = isTok TDot
comma = isTok TComma
lParen = isTok TLParen
rParen = isTok TRParen
lBrace = isTok TLBrace
rBrace = isTok TRBrace
colon = isTok TColon
equals = isTok TEquals
arrow = isTok TArrow

simpleOrOperName :: P SimpleName
simpleOrOperName = simpleName <|> (operName >>= return . SimpleName)

name :: P Name
name = do
  x <- simpleName
  xs <- many $ dot >> simpleOrOperName
  return $ Name $ x : xs

delimit :: P start -> P end -> P a -> P a
delimit start end body = do
  start
  x <- body
  end
  return x

parens :: P a -> P a
parens = delimit lParen rParen

braces :: P a -> P a
braces = delimit lBrace rBrace

staticParams = brackets $ sepEndBy param comma

params = do
  ss <- option [] staticParams
  ds <- parens $ sepEndBy param comma
  return $ Params ss ds

optionalParams = option (Params [] []) params

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
nsTypeDef :: P NsDef
nsTypeDef = do
  k  <- kind
  as <- staticArgs
  n  <- simpleName
  ps <- optionalParams
  es <- braces $ sepEndBy structElem comma
  end
  return $ NsTypeDef k as n ps es

nsTypeAlias :: P NsDef
nsTypeAlias = do
  isKw "alias"
  n  <- simpleName
  ps <- staticParams
  isTok TEquals
  t  <- expr
  return $ NsTypeAlias n ps t

isKw :: String -> P ()
isKw = isTok . TKeyword

kwTable :: [(String, a)] -> P a
kwTable = foldr1 (<|>) . map (\(k, v) -> isKw k >> return v)

kind :: P Kind
kind = kwTable
  [ ("struct", Struct)
  , ("union",  Union)
  , ("enum",   Enum)
  , ("mask",   Mask)
  ]

structElem :: P StructElem
structElem = do
  n <- simpleName
  t <- optionMaybe $ colon >> expr
  v <- optionMaybe $ equals >> expr
  return $ StructElem n t v

staticArgs = brackets $ sepEndBy expr comma

dynArgs = parens $ sepEndBy expr comma

arguments = do
  ss <- option [] staticArgs
  ds <- dynArgs
  return $ Args ss ds

loopInfoType :: P LoopInfo
loopInfoType = kwTable
  [ ("iterations", LCount)
  , ("iteration",  LPos)
  , ("first",      LFirst)
  , ("last",       LLast)
  ]

loopInfo :: P Expr
loopInfo = do
  i <- loopInfoType
  n <- simpleName
  return $ ELoopInfo i n

exprHead :: P Expr
exprHead =
  literal
  <|>
  loopInfo
  <|>
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
  [ (">=>", PCompose)
  , ("<=<", PCompose)
  , ("<=>", PCompare)
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

operName :: P String
operName =
  tok $ \t -> case t of
    TOper xs -> Just xs
    _ -> Nothing

oper :: P (Prec, Expr)
oper = do
  o <- operName
  let p = maybe PUser id $ lookup o precedence
  return (p, EName $ Name [SimpleName o])

unaryOpExpr :: P Expr
unaryOpExpr = do
  (_, o) <- oper
  e <- expr
  return $ EApp o $ Args [] [e]

type ExprTail = Either Args (Prec, Expr, Expr)

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
applyPrec h ts = EApp o $ Args [] [left', right']
  where
    minPrec = minimum $ map fst3 ts
    (left, ((_, o, e) : rTail)) = span ((< minPrec) . fst3) ts
    left' = applyPrec h left
    right' = applyPrec e rTail

literal :: P Expr
literal = tokLiteral <|> array <|> new <|> fn

tokLiteral :: P Expr
tokLiteral = tok $ \t -> case t of
  TInt n     -> Just $ EInt n
  TFloat n   -> Just $ EFloat n
  TChar c    -> Just $ EChar c
  TString xs -> Just $ EString xs
  _          -> Nothing

lBracket = isTok TLBracket

rBracket = isTok TRBracket

brackets = delimit lBracket rBracket

array :: P Expr
array = brackets $ sepEndBy expr comma >>= return . EArray

fn :: P Expr
fn = do
  ps <- params
  t  <- fnValueTail <|> fnTypeTail
  return $ t ps

fnValueTail, fnTypeTail :: P (Params -> Expr)

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
block = braces $ many stmt

loopControl kw f = do
  isKw kw
  a <- optionMaybe simpleName
  end
  return $ f a

stmt, localDef, cond, while, for, ret, decide, exec :: P Stmt
stmt =
  localDef <|> cond <|> while <|> for <|> ret <|> decide <|> exec
  <|>
  loopControl "next" SNext
  <|>
  loopControl "done" SDone
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

loopLabel = optionMaybe $ isTok TAt >> simpleName

while = do
  isKw "while"
  a <- loopLabel
  e <- expr
  b <- block
  n <- next
  return $ SWhile a e b n

{-
  The for-each loop expression must be of a type for which all the necessary
  functions are defined to allow it to be used as an iterable thing.
-}
for = do
  isKw "for"
  a <- loopLabel
  lParen
  v <- simpleName
  isKw "in"
  e <- expr
  rParen
  b <- block
  n <- next
  return $ SFor (a `mplus` Just v) v e b n

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

localDef = do
  ms     <- many $ isKw' "closed"
  let c = "closed" `elem` ms
  (n, e) <- def
  return $ SDef c n e

def = do
  isKw "def"
  n <- simpleName
  e <- defTail
  return (n, e)

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
    b  <- fnBodyWithEnd
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
  as <- dynArgs
  os <- braces $ many decision
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

nsDef = nsTypeDef <|> nsTypeAlias <|> nsValueDef

imp :: P Import
imp = do
  isKw "import"
  ns <- sepEndBy simpleOrOperName comma
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

nsValueDef :: P NsDef
nsValueDef = do
  ms <- many $ isKw' "export" <|> isKw' "global"
  let e = "export" `elem` ms
  let g = "global" `elem` ms
  (n, v) <- def
  return $ NsValueDef e g n v

