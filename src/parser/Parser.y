{
module Parser where
import Lexer
import Ast
import Data.List
}

%name parsebgl
%tokentype { Token }

%errorhandlertype explist
%error { parseError }

%monad {Alex}
%lexer{lexwrap}{TokenEOF}

%token
  type                          { TokenType pos }
  if                            { TokenIf pos }
  then                          { TokenThen pos }
  else                          { TokenElse pos }
  while                         { TokenWhile pos }
  do                            { TokenDo pos }
  of                            { TokenOf pos }
  let                           { TokenLet pos }
  in                            { TokenIn pos }
  int                           { TokenInt pos int}
  assign                        { TokenAssign pos }
  plus                          { TokenPlus pos }
  minus                         { TokenMinus pos }
  times                         { TokenTimes pos }
  div                           { TokenDiv pos }
  lparen                        { TokenLParen pos }
  rparen                        { TokenRParen pos }
  pipe                          { TokenPipe pos }
  arrow                         { TokenArrow pos }
  comma                         { TokenComa pos }
  eq                            { TokenEQ pos }
  gt                            { TokenGT pos }
  lt                            { TokenLT pos }
  lte                           { TokenLTE pos }
  gte                           { TokenGTE pos }
  symbol                        { TokenSym pos name }
  functionDef                   { TokenFunctionDef pos name }
  comment                       { TokenComment pos comment }

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




prettyParseSymbol               :: String -> String
prettyParseSymbol "type"        = "type"       
prettyParseSymbol "if"          = "if"         
prettyParseSymbol "then"        = "then"       
prettyParseSymbol "else"        = "else"       
prettyParseSymbol "while"       = "while"      
prettyParseSymbol "do"          = "do"         
prettyParseSymbol "of"          = "of"         
prettyParseSymbol "let"         = "let"        
prettyParseSymbol "in"          = "in"         
prettyParseSymbol "int"         = "Integer Type"        
prettyParseSymbol "assign"      = "="     
prettyParseSymbol "plus"        = "+"       
prettyParseSymbol "minus"       = "-"      
prettyParseSymbol "times"       = "*"      
prettyParseSymbol "div"         = "/"        
prettyParseSymbol "lparen"      = "("     
prettyParseSymbol "rparen"      = ")"     
prettyParseSymbol "pipe"        = "|"       
prettyParseSymbol "arrow"       = "->"      
prettyParseSymbol "comma"       = ","      
prettyParseSymbol "eq"          = "=="         
prettyParseSymbol "gt"          = "<"         
prettyParseSymbol "lt"          = ">"         
prettyParseSymbol "lte"         = ">="        
prettyParseSymbol "gte"         = "<="        
prettyParseSymbol "symbol"      = "symbol (Type or variable name)"     
prettyParseSymbol "functionDef" = "Function Definition"
prettyParseSymbol "comment"     = "Comment"    

formatExpected   :: [String] -> String
formatExpected e = intercalate "," $ map prettyParseSymbol e

parseError                              :: (Token, [String]) -> Alex a
parseError ((TokenEOF), expected)       = printError $ "ERROR: Unexpected End of File: " ++ formatExpected expected ++ "\n"
parseError ((TokenError p e), expected) = printError $ "ERROR: Token should have been removed in lexer: " ++ formatExpected expected ++ "\n"
parseError ((TokenWhite p w), expected) = printError $ "ERROR: Token should have been removed in lexer: "  ++ formatExpected expected ++ "\n"
parseError (tok, expected)              = printError $  "Unexpected " ++ prettyToken tok ++ "\nExpected one of the following: " ++ formatExpected expected ++ "\n"
parseError x                            = printError $ "Fatal Error: " ++ show x

parse :: String -> Either String [Stmt]
parse s = runAlex s parsebgl
}
