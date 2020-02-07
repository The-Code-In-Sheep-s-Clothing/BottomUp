{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
@symbol = $alpha [$alpha $digit \_ \']*

tokens :-
  $white+                 ;
  "--".*                  ;
  type                    { \s -> TokenType }
  if                      { \s -> TokenIf }
  then                    { \s -> TokenThen }
  else                    { \s -> TokenElse }
  while                   { \s -> TokenWhile }
  do                      { \s -> TokenDo }
  of                      { \s -> TokenOf }
  let                     { \s -> TokenLet }
  in                      { \s -> TokenIn }
  "="                     { \s -> TokenAssign }
  "+"                     { \s -> TokenPlus }
  "-"                     { \s -> TokenMinus }
  "*"                     { \s -> TokenTimes }
  "/"                     { \s -> TokenDiv }
  "("                     { \s -> TokenLParen }
  ")"                     { \s -> TokenRParen }
  "|"                     { \s -> TokenPipe }
  "->"                    { \s -> TokenArrow }
  ","                     { \s -> TokenComa }
  "=="                    { \s -> TokenEQ }
  ">"                     { \s -> TokenGT }
  "<"                     { \s -> TokenLT }
  "=<"                    { \s -> TokenLTE }
  "=>"                    { \s -> TokenGTE }
  @symbol $white* ":"     { \s -> TokenFunctionDef (stripFirstWord s) }
  @symbol                 { \s -> TokenSym s }
  $digit+                 { \s -> TokenInt (read s) }
  True                    { \s -> TokenBool True }
  Frue                    { \s -> TokenBool False }

{

-- there might be a library for this, but I dont know how to include
stripFirstWord          :: String -> String
stripFirstWord (' ':xs) = ""
stripFirstWord (x:xs)   = x : stripFirstWord xs
stripFirstWord _        = ""

-- The token type:
data Token = TokenType
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenWhile
           | TokenDo
           | TokenOf
           | TokenLet
           | TokenIn
           | TokenAssign
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           | TokenPipe
           | TokenArrow
           | TokenComa
           | TokenEQ
           | TokenGT
           | TokenLT
           | TokenLTE
           | TokenGTE
           | TokenSym String
           | TokenFunctionDef String
           | TokenInt Int
           | TokenBool Bool
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
