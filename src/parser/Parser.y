{
module Parser where
import Lexer
import Ast
}

%name parsebgl
%tokentype { Token }
%error { parseError }
%monad {Alex}
%lexer{lexwrap}{TokenEOF}

%token
  type                          { TokenType _ }
  if                            { TokenIf _ }
  then                          { TokenThen _ }
  else                          { TokenElse _ }
  while                         { TokenWhile _ }
  do                            { TokenDo _ }
  of                            { TokenOf _ }
  let                           { TokenLet _ }
  in                            { TokenIn _ }
  int                           { TokenInt _ int}
  assign                        { TokenAssign _ }
  plus                          { TokenPlus _ }
  minus                         { TokenMinus _ }
  times                         { TokenTimes _ }
  div                           { TokenDiv _ }
  lparen                        { TokenLParen _ }
  rparen                        { TokenRParen _ }
  pipe                          { TokenPipe _ }
  arrow                         { TokenArrow _ }
  comma                         { TokenComa _ }
  eq                            { TokenEQ _ }
  gt                            { TokenGT _ }
  lt                            { TokenLT _ }
  lte                           { TokenLTE _ }
  gte                           { TokenGTE _ }
  symbol                        { TokenSym _ name }
  functionDef                   { TokenFunctionDef _ name }
  comment                       { TokenComment _ comment }

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
                  
Btype         : symbol                                                    { Btype (name $1) }
Xtype         : Btype pipe XtypeHelper                                    { Xtype $1 $3 }
              | Btype                                                     { Xtype $1 []}
XtypeHelper   : symbol pipe XtypeHelper                                   { [(name $1)] ++ $3 }
              | symbol                                                    { [(name $1)] }
Ttype1        : Xtype comma Ttype1                                        { let Ttype l = $3 in Ttype $ [$1] ++ l }
              | Xtype comma Xtype                                         { Ttype $ [$1] ++ [$3] }
Ttype         : lparen Ttype1 rparen                                      { $2 }
Ptype         : Ttype                                                     { Ttype' $1 }
              | Xtype                                                     { Xtype' $1 }
Ftype         : Ptype arrow Ptype                                         { Ftype $1 $3 }
Type          : Ptype                                                     { Ptype' $1 }
              | Ftype                                                     { Ftype' $1 }
                  
Signature     : functionDef Type                                          { Signature (name $1) $2}
Equation      : symbol OptionalList assign WeakStmt                       { Equation (name $1) $2 $4 }
Equations     : Equation Equations                                        { [$1] ++ $2 }
              | Equation                                                  { [$1] }
                
Stmt          : Signature Equations                                       { Valdef $1 $2}
              | type symbol assign Type                                   { Typedef (name $2) $4 }
              | type symbol assign FunctionApp of Type                    { TypedefFunc (name $2) $4 $6 }
              | comment                                                   { SComment (comment $1) }
                
                
WeakStmt      : if Expr then WeakStmt else WeakStmt                       { Conditional $2 $4 $6 }
              | let symbol assign Expr in WeakStmt                        { Let (name $2) $4 $6 }
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
                              
Variable      : int                                                       { EInt (int $1) }
              | symbol                                                    { ESymbol (name $1) }
              | FunctionApp                                               { $1 }
              | lparen TupleList rparen                                   { Tuple $2 }
                              
FunctionApp   : symbol OptionalArgs                                       { FunctionApp (name $1) $2 }

{

parseError :: Token -> Alex a
parseError _ = alexError "Unexpected end of Input"

parse :: String -> Either String [Stmt]
parse s = runAlex s parsebgl
}
