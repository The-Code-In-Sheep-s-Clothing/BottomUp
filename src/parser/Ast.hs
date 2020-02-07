module Ast where

data Signature = Signature String Type
               deriving Show
data Equation = Equation String Expr Stmt
              deriving Show

data Binop = Plus
           | Minus
           | Times
           | Div
           | EqualTo
           | LessThan
           | GreaterThan
           | LessThanEqual
           | GreaterThanEqual
           deriving Show

data Stmt = Conditional Expr Stmt Stmt
          | Let String Expr Stmt
          | While Expr Expr
          | Valdef Signature [Equation]
          | Typedef String Type
          | TypedefFunc String Expr Type -- Used for type Board = Grid() of ...
          | SExpr Expr
          | SComment String
          deriving Show

data Expr = EInt Int
          | ESymbol String
          | Paren Expr
          | Tuple [Expr]
          | FunctionApp String Expr
          | Infix Expr Binop Expr
          | Empty
          deriving Show

data Btype = Btype String
           | Ttype'' Ttype 
           deriving Show

data Xtype = Xtype Btype [String]
           deriving Show

data Ttype = Ttype [Xtype]
            deriving Show

data Ptype = Xtype' Xtype
           | Ttype' Ttype
           deriving Show

data Ftype = Ftype Ptype Ptype
           deriving Show

data Type = Ptype' Ptype
          | Ftype' Ftype
          deriving Show
