
module AST where

import Text.Parsec

import Lexer

type P = Parsec [(Token, SourcePos, Maybe String)] ()

data L a = L
  { loc :: SourcePos
  , val :: a
  } deriving (Eq, Show)

vals :: (Functor f) => f (L a) -> f a
vals = fmap val

data SimpleName = SimpleName String deriving (Eq, Show)

data Name = Name [L SimpleName] deriving (Eq, Show)

data Param =
    TypeParam SimpleName
  | ValueParam SimpleName Type
  deriving (Eq, Show)

type Type = Expr

data Expr =
    EApp (L Expr) Args
  | EInt Integer
  | EFloat Double
  | EChar Char
  | EString String
  | EName Name
  | EArray [L Expr]
  | ENew (L Name) Args
  | EFnValue Params (L Type) (L FnBody)
  | EFnType Params (L Type)
  | ELoopInfo LoopInfo (L SimpleName)
  deriving (Eq, Show)

data Args = Args [L Expr] [L Expr] deriving (Eq, Show)

data Params =
  Params
  { pStatics  :: [L Param]
  , pDynamics :: [L Param]
  } deriving (Eq, Show)

data LoopInfo = LFirst | LLast | LPos | LCount deriving (Eq, Show)

type FnBody = Either Block Expr

type Block = [L Stmt]

type LoopLabel = Maybe (L SimpleName)

data Stmt =
    SExpr Expr
  | SDef Bool (L SimpleName) (L Expr)
  | SIf (Maybe (L Expr)) (L Expr) Block [(L Expr, Block)] (Maybe Block)
  | SWhile LoopLabel (L Expr) Block (Maybe Block)
  | SFor LoopLabel (L SimpleName) (L Expr) Block (Maybe Block)
  | SNext LoopLabel
  | SDone LoopLabel
  | SReturn (Maybe (L Expr))
  | SDecide [L Expr] [([Maybe (L Expr)], Block)]
  | SDo Block
  | SEmpty
  deriving (Eq, Show)

data Namespace = Namespace (L Name) [L Import] [L NsDef] deriving (Eq, Show)

data Kind = Struct | Union | Enum | Mask deriving (Eq, Show)

data StructElem =
  StructElem
  { seName :: L SimpleName
  , seType :: (Maybe Type)
  , seInit :: (Maybe Expr)
  } deriving (Eq, Show)

data NsDef =
    NsValueDef
    { vdExport :: Bool
    , vdGlobal :: Bool
    , vdName   :: L SimpleName
    , vdValue  :: L Expr
    }
  | NsTypeDef
    { tdKind   :: Kind
    , tdArgs   :: [L Expr]
    , tdName   :: L SimpleName
    , tdParams :: Params
    , tdElems  :: [StructElem]
    }
  | NsTypeAlias
    { taName   :: L SimpleName
    , taParams :: [L Param]
    , taType   :: L Type
    }
  deriving (Eq, Show)

data Import =
  Import
  { iFrom  :: L Name
  , iNames :: (Maybe [L SimpleName])
  } deriving (Eq, Show)

