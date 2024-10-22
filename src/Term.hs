module Term
  ( ArithBOp (..)
  , ArithUOp (..)
  , Expr (..)
  , Stat (..)
  , Type (..)
  , TopFunc (..)
  ) where

data ArithUOp =
    UNeg
  | UNot
  deriving Show

data ArithBOp =
    BAdd
  | BSub
  | BMul
  | BDiv
  | BEq
  | BNe
  | BLt
  | BLe
  | BGt
  | BGe
  | BAnd
  | BOr
  | BXor
  deriving Show

data Expr =
    EInt Int
  | EUOp ArithUOp Expr
  | EBOp ArithBOp Expr Expr
  | EChoose Expr Expr
  | EAgg [Expr]
  | EProj Expr Int
  | EList [Expr]
  | EIndex Expr Expr
  | EVar String
  | ECall Expr [Expr]
  deriving Show

data Stat =
    SReturn (Maybe Expr)
  | SIf Expr [Stat] [Stat]
  | SWhile Expr [Stat]
  | SChoose [Stat] [Stat]
  | SBlock [Stat]
  | SCall Expr [Expr]
  | SSet String Expr
  deriving Show

data Type =
    TUnit
  | TFunc [Type] Type
  | TInt
  | TAgg [Type]
  | TList Type
  deriving (Show, Eq)

data TopFunc = TopFunc
  { name :: String
  , args :: [(String, Type)]
  , ret  :: Type
  , stats :: [Stat] }

{-

fn my_func(a, b, c):
  choose:
    a
    b
  or:
    c
    d
  block:
    
  a
  b
  c
-}

