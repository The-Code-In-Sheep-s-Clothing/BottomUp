{
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
  type                          { TokenType }
  if                            { TokenIf }
  then                          { TokenThen }
  else                          { TokenElse }
  while                         { TokenWhile }
  do                            { TokenDo }
  int                           { TokenInt $$ }
  assign                        { TokenAssign }
  plus                          { TokenPlus }
  minus                         { TokenMinus }
  times                         { TokenTimes }
  div                           { TokenDiv }
  lparen                        { TokenLParen }
  rparen                        { TokenRParen }
  pipe                          { TokenPipe }
  colon                         { TokenColon }
  arrow                         { TokenArrow }
  comma                         { TokenComa }
  eq                            { TokenEQ }
  gt                            { TokenGT }
  lt                            { TokenLT }
  lte                           { TokenLTE }
  gte                           { TokenGTE }
  symbol                        { TokenSym $$ }

%left eq gt lt lte gte
%left plus minus 
%left times div
%left assign

%%
List : Expr comma ListHelper { [$1] ++ $3 }
ListHelper : Expr comma ListHelper { [$1] ++ $3 }
           | Expr { [$1] }

Args : Expr comma Args { [$1] ++ $3 }
     | Expr { [$1] }

Stmt : if Expr then Stmt else Stmt  { Conditional $2 $4 $6 }
      | while Expr do Stmt          { While $2 $4 }
      | Expr                        { $1 }

Expr : int                          { EInt $1 }
     | symbol                       { ESymbol $1 }
     | lparen Expr rparen           { Paren $2 }
     | lparen List rparen           { Tuple $2 }
     | symbol lparen Args rparen    { FunctionApp $1 $3 }
     | Expr plus Expr               { Infix $1 Plus $3 }
     | Expr minus Expr              { Infix $1 Minus $3 }
     | Expr times Expr              { Infix $1 Times $3 }
     | Expr div Expr                { Infix $1 Div $3 }
     | Expr eq Expr                 { Infix $1 EqualTo $3 }
     | Expr gt Expr                 { Infix $1 GreaterThan $3 }
     | Expr lt Expr                 { Infix $1 LessThan $3 }
     | Expr lte Expr                { Infix $1 LessThanEqual $3 }
     | Expr gte Expr                { Infix $1 GreaterThanEqual $3 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Valdef = Valdef Signature [Equation]
data Signature = Signature String Type
data Equation = Equation String Expr

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

data Expr = EInt Int
          | ESymbol String
          | Paren Expr
          | Tuple [Expr]
          | FunctionApp String [Expr]
          | Infix Expr Binop Expr
          | Conditional Expr Expr Expr
          | While Expr Expr
          | Empty
          deriving Show

data Btype = Bool String
           | TInt String
           | TSymbol String
           deriving Show

data Xtype = Xtype Btype String
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

}