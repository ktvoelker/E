
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
    EApp Expr [Expr]
  | EInt Integer
  | EFloat Double
  | EChar Char
  | EString String
  | EName Name
  | EArray [Expr]
  | ENew Name [Expr]
  | EFnValue [Param] Type FnBody
  | EFnType [Param] Type
  deriving (Eq, Show)

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

data StructElem = StructElem SimpleName (Maybe Type) (Maybe Expr) deriving (Eq, Show)

data NsDef =
    NsValueDef Bool Bool SimpleName Expr
  | NsTypeDef Kind [Expr] SimpleName [Param] [StructElem]
  | NsTypeAlias SimpleName [Param] Type
  deriving (Eq, Show)

data Import = Import Name (Maybe [SimpleName]) deriving (Eq, Show)

