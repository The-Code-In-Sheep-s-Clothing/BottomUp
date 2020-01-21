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
  of                            { TokenOf }
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
  functionDef                   { TokenFunctionDef $$ }

%left eq gt lt lte gte
%left plus minus 
%left times div
%left assign

%%
Stmts : Stmt Stmts {[$1] ++ $2}
      | Stmt {[$1]}

List : Variable comma ListHelper { [$1] ++ $3 }
ListHelper : Variable comma ListHelper { [$1] ++ $3 }
           | Variable { [$1] }

-- TODO Can't parse tuples as args
OptionalArgs : lparen Args rparen {$2}
             | {[]}
Args : Variable comma Args { [$1] ++ $3 }
     | Variable { [$1] }

Btype : symbol {Btype $1}
Xtype : Btype pipe Xtype { let Xtype l = $3 in Xtype $ [$1] ++ l }
      | Btype { Xtype [$1] }
Ttype1 : Xtype comma Ttype1 { let Ttype l = $3 in Ttype $ [$1] ++ l }
       | Xtype comma Xtype { Ttype $ [$1] ++ [$3] }
Ttype : lparen Ttype1 rparen { $2 }
Ptype : Ttype { Ttype' $1 }
      | Xtype { Xtype' $1 }
Ftype : Ptype arrow Ptype { Ftype $1 $3 }
Type : Ptype { Ptype' $1 }
     | Ftype { Ftype' $1 }

Signature : functionDef Type { Signature $1 $2}
Equation : symbol OptionalArgs assign WeakStmt { Equation $1 $2 $4 }
Equations : Equation Equations {[$1] ++ $2}
          | Equation {[$1]}

Stmt : Signature Equations                        { Valdef $1 $2}
     | type symbol assign Type                    { Typedef $2 $4 }
     | type symbol assign FunctionApp of Type     { TypedefFunc $2 $4 $6}
     

WeakStmt : if Expr then Expr else Expr  { Conditional $2 $4 $6 }
         | while Expr do Expr           { While $2 $4 }
         | Expr                         { SExpr $1 }

Expr : Variable                     { $1 }
     | lparen Expr rparen           { Paren $2 }
     | lparen List rparen           { Tuple $2 }
     | Expr plus Expr               { Infix $1 Plus $3 }
     | Expr minus Expr              { Infix $1 Minus $3 }
     | Expr times Expr              { Infix $1 Times $3 }
     | Expr div Expr                { Infix $1 Div $3 }
     | Expr eq Expr                 { Infix $1 EqualTo $3 }
     | Expr gt Expr                 { Infix $1 GreaterThan $3 }
     | Expr lt Expr                 { Infix $1 LessThan $3 }
     | Expr lte Expr                { Infix $1 LessThanEqual $3 }
     | Expr gte Expr                { Infix $1 GreaterThanEqual $3 }

Variable : int                          { EInt $1 }
         | symbol                       { ESymbol $1 }
         | FunctionApp                  { $1 }

FunctionApp : symbol lparen Args rparen    { FunctionApp $1 $3 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Signature = Signature String Type
               deriving Show
data Equation = Equation String [Expr] Stmt
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

data Stmt = Conditional Expr Expr Expr
          | While Expr Expr
          | Valdef Signature [Equation]
          | Typedef String Type
          | TypedefFunc String Expr Type -- Used for type Board = Grid() of ...
          | SExpr Expr
          deriving Show

data Expr = EInt Int
          | ESymbol String
          | Paren Expr
          | Tuple [Expr]
          | FunctionApp String [Expr]
          | Infix Expr Binop Expr
          | Empty
          deriving Show

data Btype = Btype String
           deriving Show

data Xtype = Xtype [Btype]
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