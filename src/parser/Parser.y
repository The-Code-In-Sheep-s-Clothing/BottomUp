{
module Parser where
import Lexer
import Ast
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
  let                           { TokenLet }
  in                            { TokenIn }
  int                           { TokenInt $$ }
  assign                        { TokenAssign }
  plus                          { TokenPlus }
  minus                         { TokenMinus }
  times                         { TokenTimes }
  div                           { TokenDiv }
  lparen                        { TokenLParen }
  rparen                        { TokenRParen }
  pipe                          { TokenPipe }
  arrow                         { TokenArrow }
  comma                         { TokenComa }
  eq                            { TokenEQ }
  gt                            { TokenGT }
  lt                            { TokenLT }
  lte                           { TokenLTE }
  gte                           { TokenGTE }
  symbol                        { TokenSym $$ }
  functionDef                   { TokenFunctionDef $$ }
  comment                       { TokenComment $$ }

%left eq gt lt lte gte
%left plus minus
%left times div
%left assign

%%
Stmts         : Stmt Stmts                                                { [$1] ++ $2 }
              | Stmt                                                      { [$1] }

OptionalArgs  : lparen TupleList rparen                                   { Tuple $2 }
              | lparen Variable rparen                                    { $2 }
OptionalList  : lparen TupleList rparen                                   { Tuple $2 }
              | lparen Variable rparen                                    { $2 }
              |                                                           { Empty }
TupleList     : Variable comma List                                       { [$1] ++ $3 }
List          : Variable comma List                                       { [$1] ++ $3 }
              | Variable                                                  { [$1] }
                  
Btype         : symbol                                                    { Btype $1 }
Xtype         : Btype pipe XtypeHelper                                    { Xtype $1 $3 }
              | Btype                                                     { Xtype $1 []}
XtypeHelper   : symbol pipe XtypeHelper                                   { [$1] ++ $3 }
              | symbol                                                    { [$1] }
Ttype1        : Xtype comma Ttype1                                        { let Ttype l = $3 in Ttype $ [$1] ++ l }
              | Xtype comma Xtype                                         { Ttype $ [$1] ++ [$3] }
Ttype         : lparen Ttype1 rparen                                      { $2 }
Ptype         : Ttype                                                     { Ttype' $1 }
              | Xtype                                                     { Xtype' $1 }
Ftype         : Ptype arrow Ptype                                         { Ftype $1 $3 }
Type          : Ptype                                                     { Ptype' $1 }
              | Ftype                                                     { Ftype' $1 }
                  
Signature     : functionDef Type                                          { Signature $1 $2}
Equation      : symbol OptionalList assign WeakStmt                       { Equation $1 $2 $4 }
Equations     : Equation Equations                                        { [$1] ++ $2 }
              | Equation                                                  { [$1] }
                
Stmt          : Signature Equations                                       { Valdef $1 $2}
              | type symbol assign Type                                   { Typedef $2 $4 }
              | type symbol assign FunctionApp of Type                    { TypedefFunc $2 $4 $6 }
              | comment                                                   { SComment $1 }
                
                
WeakStmt      : if Expr then WeakStmt else WeakStmt                       { Conditional $2 $4 $6 }
              | let symbol assign Expr in WeakStmt                        { Let $2 $4 $6 }
              | while Expr do Expr                                        { While $2 $4 }
              | Expr                                                      { SExpr $1 }
                              
Expr          : Variable                                                  { $1 }
              | lparen Expr rparen                                        { Paren $2 }
              | Expr plus Expr                                            { Infix $1 Plus $3 }
              | Expr minus Expr                                           { Infix $1 Minus $3 }
              | Expr times Expr                                           { Infix $1 Times $3 }
              | Expr div Expr                                             { Infix $1 Div $3 }
              | Expr eq Expr                                              { Infix $1 EqualTo $3 }
              | Expr gt Expr                                              { Infix $1 GreaterThan $3 }
              | Expr lt Expr                                              { Infix $1 LessThan $3 }
              | Expr lte Expr                                             { Infix $1 LessThanEqual $3 }
              | Expr gte Expr                                             { Infix $1 GreaterThanEqual $3 }
                              
Variable      : int                                                       { EInt $1 }
              | symbol                                                    { ESymbol $1 }
              | FunctionApp                                               { $1 }
              | lparen TupleList rparen                                   { Tuple $2 }
                              
FunctionApp   : symbol OptionalArgs                                       { FunctionApp $1 $2 }

{

parseError :: [Token] -> a
parseError (l:ls) = error $ "error: " ++ (show l)
parseError [] = error "Unexpected end of Input"
}
