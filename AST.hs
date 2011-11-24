
module AST where

import Text.Parsec

import Lexer

type P = Parsec [(Token, SourcePos, Maybe String)] ()

data SimpleName = SimpleName String deriving (Eq, Show)

data Name = Name [SimpleName] deriving (Eq, Show)

data Param =
    TypeParam SimpleName
  | ValueParam SimpleName Type
  deriving (Eq, Show)

type Type = Expr

data Expr =
    EApp Expr Args
  | EInt Integer
  | EFloat Double
  | EChar Char
  | EString String
  | EName Name
  | EArray [Expr]
  | ENew Name Args
  | EFnValue Params Type FnBody
  | EFnType Params Type
  | ELoopInfo LoopInfo SimpleName
  deriving (Eq, Show)

data Args = Args [Expr] [Expr] deriving (Eq, Show)

data Params =
  Params
  { pStatics  :: [Param]
  , pDynamics :: [Param]
  } deriving (Eq, Show)

data LoopInfo = LFirst | LLast | LPos | LCount deriving (Eq, Show)

type FnBody = Either Block Expr

type Block = [Stmt]

type LoopLabel = Maybe SimpleName

data Stmt =
    SExpr Expr
  | SDef Bool SimpleName Expr
  | SIf (Maybe Expr) Expr Block [(Expr, Block)] (Maybe Block)
  | SWhile LoopLabel Expr Block (Maybe Block)
  | SFor LoopLabel SimpleName Expr Block (Maybe Block)
  | SNext LoopLabel
  | SDone LoopLabel
  | SReturn (Maybe Expr)
  | SDecide [Expr] [([Maybe Expr], Block)]
  | SDo Block
  | SEmpty
  deriving (Eq, Show)

data Namespace = Namespace Name [Import] [NsDef] deriving (Eq, Show)

data Kind = Struct | Union | Enum | Mask deriving (Eq, Show)

data StructElem =
  StructElem
  { seName :: SimpleName
  , seType :: (Maybe Type)
  , seInit :: (Maybe Expr)
  } deriving (Eq, Show)

data NsDef =
    NsValueDef
    { vdExport :: Bool
    , vdGlobal :: Bool
    , vdName   :: SimpleName
    , vdValue  :: Expr
    }
  | NsTypeDef
    { tdKind   :: Kind
    , tdArgs   :: [Expr]
    , tdName   :: SimpleName
    , tdParams :: Params
    , tdElems  :: [StructElem]
    }
  | NsTypeAlias
    { taName   :: SimpleName
    , taParams :: [Param]
    , taType   :: Type
    }
  deriving (Eq, Show)

data Import =
  Import
  { iFrom  :: Name
  , iNames :: (Maybe [SimpleName])
  } deriving (Eq, Show)

