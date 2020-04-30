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
  int                           { TokenInt pos int }
  assign                        { TokenAssign pos }
  plus                          { TokenPlus pos }
  minus                         { TokenMinus pos }
  times                         { TokenTimes pos }
  div                           { TokenDiv pos }
  lparen                        { TokenLParen pos }
  rparen                        { TokenRParen pos }
  lcurly                        { TokenLCurly pos }
  rcurly                        { TokenRCurly pos }
  arrow                         { TokenArrow pos }
  comma                         { TokenComa pos }
  eq                            { TokenEQ pos }
  gt                            { TokenGT pos }
  lt                            { TokenLT pos }
  lte                           { TokenLTE pos }
  gte                           { TokenGTE pos }
  symbol                        { TokenSym pos name }
  typename                      { TokenTypeName pos name }
  functionDef                   { TokenFunctionDef pos name }
  comment                       { TokenComment pos comment }
  colon                         { TokenColon pos }
  amp                           { TokenAmp pos }
  bang                          { TokenBang pos }

%left eq gt lt lte gte bang
%left plus minus
%left times div
%left assign

%%
Stmts         : Stmt Stmts                                                { [$1] ++ $2 }
              | Stmt                                                      { [$1] }

RequiredArgs  : TupleList                                                 { ETuple $1 (tuplePosition $1) }
              | lparen Expr rparen                                    { $2 }
OptionalArgs  : TupleList                                                 { ETuple $1 (tuplePosition $1) }
              | lparen Expr rparen                                    { $2 }
              |                                                           { Empty }

TupleList     : lparen TupleElement comma TupleHelper rparen              { TupleList ([$2] ++ $4) (tokPosition $1)}
TupleHelper   : TupleElement comma TupleHelper                            { [$1] ++ $3}
              | TupleElement                                              { [$1] }
TupleElement  : WeakVariable                                              { TupleValue $1 (exprPosition $1) }
              | TupleList                                                 { $1 }
                  
Btype         : typename                                                  { Btype (name $1) (tokPosition $1) }
Etype         : lcurly EtypeHelper rcurly amp Etype                       { $2 ++ $5 }
              | lcurly EtypeHelper rcurly                                 { $2 }
EtypeHelper   : typename comma EtypeHelper                                { [$1] ++ $3 }  
              | typename                                                  { [$1] }
Xtype         : Btype amp Etype                                           { Xtype $1 (map name $3) (bbypePosition $1) }
              | Btype                                                     { Xtype $1 [] (bbypePosition $1) }
              | Etype                                                     { Etype (map name $1) (tokPosition ($1 !! 0))}
Ttype         : lparen Ptype comma TtypeHelper rparen                     { Ttype ([$2] ++ $4) (tokPosition $1) }
TtypeHelper   : Ptype comma TtypeHelper                                   { [$1] ++ $3 }
              | Ptype                                                     { [$1] }
Ptype         : Ttype                                                     { Ttype' $1 (ttypePosition $1) }
              | Xtype                                                     { Xtype' $1 (xtypePosition $1) }
Ftype         : Ptype arrow Ptype                                         { Ftype $1 $3 (ptypePosition $1) }
Type          : Ptype                                                     { Ptype' $1 (ptypePosition $1) }
              | Ftype                                                     { Ftype' $1 (ftypePosition $1) }
                  
Signature     : functionDef colon Type                                    { Signature (name $1) $3 (tokPosition $1) }
Equation      : symbol OptionalArgs assign WeakStmt                       { Equation (name $1) $2 $4 (tokPosition $1) }
ArrayEquation : symbol bang TupleList assign WeakStmt                     { ArrayEquation (name $1) (ETuple $3 (tuplePosition $3)) $5 (tokPosition $1) }
ArrayEquations: ArrayEquation ArrayEquations                              { [$1] ++ $2 }
              | ArrayEquation                                             { [$1] }
Equations     : Equation                                                  { [$1] }
              | ArrayEquations                                            { $1 }
                
Stmt          : Signature Equations                                       { Valdef $1 $2 (sigPosition $1)}
              | type typename assign Type                                 { Typedef (name $2) $4 (tokPosition $1) }
              | type typename assign typename RequiredArgs of Type        { TypedefFunc (name $2) (FunctionApp (name $4) $5 (tokPosition $4)) $7 (tokPosition $1) }
              | comment                                                   { SComment (comment $1) (tokPosition $1) }
                
                
WeakStmt      : if Expr then WeakStmt else WeakStmt                       { Conditional $2 $4 $6 (tokPosition $1) }
              | let symbol assign Expr in WeakStmt                        { Let (name $2) $4 $6 (tokPosition $1) }
              | while Expr do Expr                                        { While $2 $4 (tokPosition $1) }
              | Expr                                                      { SExpr $1 (exprPosition $1) }
                              
Expr          : Variable                                                  { $1 }
              | lparen Expr rparen                                        { Paren $2 (tokPosition $1) }
              | Expr plus Expr                                            { Infix $1 (Plus $ tokPosition $2) $3 (exprPosition $1) }
              | Expr minus Expr                                           { Infix $1 (Minus $ tokPosition $2) $3 (exprPosition $1) }
              | Expr times Expr                                           { Infix $1 (Times $ tokPosition $2) $3 (exprPosition $1) }
              | Expr div Expr                                             { Infix $1 (Div $ tokPosition $2) $3 (exprPosition $1) }
              | Expr eq Expr                                              { Infix $1 (EqualTo $ tokPosition $2) $3 (exprPosition $1) }
              | Expr gt Expr                                              { Infix $1 (GreaterThan $ tokPosition $2) $3 (exprPosition $1) }
              | Expr lt Expr                                              { Infix $1 (LessThan $ tokPosition $2) $3 (exprPosition $1) }
              | Expr lte Expr                                             { Infix $1 (LessThanEqual $ tokPosition $2) $3 (exprPosition $1) }
              | Expr gte Expr                                             { Infix $1 (GreaterThanEqual $ tokPosition $2) $3 (exprPosition $1) }
              | Expr bang Expr                                            { Infix $1 (Bang $ tokPosition $2) $3 (exprPosition $1) }
                              
Variable      : WeakVariable                                              { $1 }
              | TupleList                                                 { ETuple $1 (tuplePosition $1)}
                              
WeakVariable  : int                                                       { EInt (int $1) (tokPosition $1)}
              | symbol                                                    { ESymbol (name $1) (tokPosition $1)}
              | typename                                                  { ESymbol (name $1) (tokPosition $1)} -- this WILL change
              | FunctionApp                                               { $1 }
                          
FunctionApp   : symbol RequiredArgs                                       { FunctionApp (name $1) $2 (tokPosition $1) }

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
prettyParseSymbol "bang"        = "!"
prettyParseSymbol "lcurly"      = "{"
prettyParseSymbol "rcurly"      = "}"
prettyParseSymbol "amp"         = "&"
prettyParseSymbol str           = str

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
