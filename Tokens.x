{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+                       ;
  "--".*                        ;
  type                          { \s -> TokenType }
  if                            { \s -> TokenIf }
  then                          { \s -> TokenThen }
  else                          { \s -> TokenElse }
  while                         { \s -> TokenWhile }
  do                            { \s -> TokenDo }
  \=                            { \s -> TokenAssign }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \|                            { \s -> TokenPipe }
  \:                            { \s -> TokenColon }
  \-\>                          { \s -> TokenArrow }
  \,                            { \s -> TokenComa }
  \=\=                          { \s -> TokenEQ }
  \>                            { \s -> TokenGT }
  \<                            { \s -> TokenLT }
  \=\<                          { \s -> TokenLTE }
  \=\>                          { \s -> TokenGTE }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }
  $digit+                  { \s -> TokenInt (read s) }
  True                          { \s -> TokenBool True }
  Frue                          { \s -> TokenBool False }

{

-- The token type:
data Token = TokenType
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenWhile
           | TokenDo
           | TokenAssign
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           | TokenPipe
           | TokenColon
           | TokenArrow
           | TokenComa
           | TokenEQ
           | TokenGT
           | TokenLT
           | TokenLTE
           | TokenGTE
           | TokenSym String
           | TokenInt Int
           | TokenBool Bool
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
