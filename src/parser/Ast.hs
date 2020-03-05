{-# LANGUAGE DuplicateRecordFields #-}
module Ast where

import Lexer

data Signature      = Signature         {   functionName    :: String
                                        ,   functionType    :: Type
                                        ,   sigPosition     :: AlexPosn
                                        }   
                    deriving Show

data Equation       = Equation          {   functionName    :: String
                                        ,   functionArgs    :: Expr
                                        ,   functionStmt    :: Stmt
                                        ,   equPosition     :: AlexPosn
                                        }
                    | ArrayEquation     {   functionName    :: String
                                        ,   functionArgs    :: Expr
                                        ,   functionStmt    :: Stmt
                                        ,   equPosition     :: AlexPosn
                                        }  
                    deriving Show

data Binop          = Plus              {   binopPosition   :: AlexPosn }
                    | Minus             {   binopPosition   :: AlexPosn }
                    | Times             {   binopPosition   :: AlexPosn }
                    | Div               {   binopPosition   :: AlexPosn }
                    | EqualTo           {   binopPosition   :: AlexPosn }
                    | LessThan          {   binopPosition   :: AlexPosn }
                    | GreaterThan       {   binopPosition   :: AlexPosn }
                    | LessThanEqual     {   binopPosition   :: AlexPosn }
                    | GreaterThanEqual  {   binopPosition   :: AlexPosn }
                    | Bang              {   binopPosition   :: AlexPosn }
                    deriving Show

data Stmt           = Conditional       {   condition       :: Expr
                                        ,   ifthen          :: Stmt
                                        ,   elsethen        :: Stmt
                                        ,   stmtPosition    :: AlexPosn
                                        }
                    | Let               {   variable        :: String 
                                        ,   letExpr         :: Expr
                                        ,   follows         :: Stmt
                                        ,   stmtPosition    :: AlexPosn
                                        }
                    | While             {   condition       :: Expr
                                        ,   application     :: Expr
                                        ,   stmtPosition    :: AlexPosn
                                        }
                    | Valdef            {   sig             :: Signature
                                        ,   patterns        :: [Equation]
                                        ,   stmtPosition    :: AlexPosn
                                        }
                    | Typedef           {   typeName        :: String
                                        ,   oldType         :: Type
                                        ,   stmtPosition    :: AlexPosn
                                        }
                    | TypedefFunc       {   functionName    :: String
                                        ,   oldFunction     :: Expr
                                        ,   functionType    :: Type
                                        ,   stmtPosition    :: AlexPosn
                                        }
                    | SExpr             {   stmtExpr        :: Expr
                                        ,   stmtPosition    :: AlexPosn
                                        }
                    | SComment          {   commentText     :: String
                                        ,   stmtPosition    :: AlexPosn
                                        }
                    deriving Show

data Tuple          = TupleList         {   elements        :: [Tuple]
                                        ,   tuplePosition   :: AlexPosn
                                        }
                    | TupleValue        {   tupleValue      :: Expr
                                        ,   tuplePosition   :: AlexPosn
                                        }
                    deriving Show

data Expr           = EInt              {   value           :: Int
                                        ,   exprPosition    :: AlexPosn
                                        }
                    | ESymbol           {   symbolName      :: String
                                        ,   exprPosition    :: AlexPosn
                                        }
                    | Paren             {   parenExpr       :: Expr
                                        ,   exprPosition    :: AlexPosn
                                        }
                    | ETuple            {   values          :: Tuple
                                        ,   exprPosition    :: AlexPosn
                                        }
                    | FunctionApp       {   functionName    :: String
                                        ,   functionArgs    :: Expr
                                        ,   exprPosition    :: AlexPosn
                                        }
                    | Infix             {   lExpr           :: Expr
                                        ,   operation       :: Binop
                                        ,   rExpr           :: Expr
                                        ,   exprPosition    :: AlexPosn
                                        }
                    | Empty
                    deriving Show

data Btype          = Btype             {   typeName        :: String
                                        ,   bbypePosition   :: AlexPosn
                                        }   
                    deriving Show

data Xtype          = Xtype             {   btype           :: Btype
                                        ,   extensions      :: [String]
                                        ,   xtypePosition   :: AlexPosn
                                        }   
                    deriving Show

data Ttype          = TtypeValue        {   tupleval       :: Xtype
                                        ,   ttypePosition   :: AlexPosn
                                        }
                    | TtypeList         {   tupleelements   :: [Ttype]
                                        ,   ttypePosition   :: AlexPosn
                                        }   
                    deriving Show

data Ptype          = Xtype'            {   xtype           :: Xtype
                                        ,   ptypePosition   :: AlexPosn
                                        }
                    | Ttype'            {   ttype           :: Ttype
                                        ,   ptypePosition   :: AlexPosn
                                        }
                    deriving Show

data Ftype          = Ftype             {   ltype           :: Ptype
                                        ,   rtype           :: Ptype
                                        ,   ftypePosition   :: AlexPosn
                                        }
                    deriving Show

data Type           = Ptype'            {   ptype           :: Ptype
                                        ,   typePosition    :: AlexPosn
                                        }
                    | Ftype'            {   ftype           :: Ftype
                                        ,   typePosition    :: AlexPosn
                                        }
                    deriving Show

