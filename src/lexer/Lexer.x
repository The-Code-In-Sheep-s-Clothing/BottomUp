{
module Lexer where
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
@symbol = $alpha [$alpha $digit \_ \']*

tokens :-
  $white+                 ;
  "--".*                  { mkL LexComment }
  type                    { mkL LexType }
  if                      { mkL LexIf }
  then                    { mkL LexThen }
  else                    { mkL LexElse }
  while                   { mkL LexWhile }
  do                      { mkL LexDo }
  of                      { mkL LexOf }
  let                     { mkL LexLet }
  in                      { mkL LexIn }
  "="                     { mkL LexAssign }
  "+"                     { mkL LexPlus }
  "-"                     { mkL LexMinus }
  "*"                     { mkL LexTimes }
  "/"                     { mkL LexDiv }
  "("                     { mkL LexLParen }
  ")"                     { mkL LexRParen }
  "|"                     { mkL LexPipe }
  "->"                    { mkL LexArrow }
  ","                     { mkL LexComa }
  "=="                    { mkL LexEQ }
  ">"                     { mkL LexGT }
  "<"                     { mkL LexLT }
  "=<"                    { mkL LexLTE }
  "=>"                    { mkL LexGTE }
  @symbol $white* ":"     { mkL LexFunctionDef }
  @symbol                 { mkL LexSym }
  $digit+                 { mkL LexInt }
  True                    { mkL LexBool }
  Frue                    { mkL LexBool }

{

-- there might be a library for this, but I dont know how to include
stripFirstWord          :: String -> String
stripFirstWord (' ':xs) = ""
stripFirstWord (x:xs)   = x : stripFirstWord xs
stripFirstWord _        = ""

-- The token type:
data Token    = TokenComment {position :: AlexPosn, comment :: String}
              | TokenType {position :: AlexPosn}
              | TokenIf {position :: AlexPosn}
              | TokenThen {position :: AlexPosn}
              | TokenElse {position :: AlexPosn}
              | TokenWhile {position :: AlexPosn}
              | TokenDo {position :: AlexPosn}
              | TokenOf {position :: AlexPosn}
              | TokenLet {position :: AlexPosn}
              | TokenIn {position :: AlexPosn}
              | TokenAssign {position :: AlexPosn}
              | TokenPlus {position :: AlexPosn}
              | TokenMinus {position :: AlexPosn}
              | TokenTimes {position :: AlexPosn}
              | TokenDiv {position :: AlexPosn}
              | TokenLParen {position :: AlexPosn}
              | TokenRParen {position :: AlexPosn}
              | TokenPipe {position :: AlexPosn}
              | TokenArrow {position :: AlexPosn}
              | TokenComa {position :: AlexPosn}
              | TokenEQ {position :: AlexPosn}
              | TokenGT {position :: AlexPosn}
              | TokenLT {position :: AlexPosn}
              | TokenLTE {position :: AlexPosn}
              | TokenGTE {position :: AlexPosn}
              | TokenSym {position :: AlexPosn, name :: String}
              | TokenFunctionDef {position :: AlexPosn, name :: String}
              | TokenInt {position :: AlexPosn, int :: Int}
              | TokenBool {position :: AlexPosn, bool :: Bool}
              | TokenEOF
              deriving (Eq,Show)

mkL :: LexClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, _, str) len = let t = take len str
                          in case c of
                            LexComment -> return (TokenComment p t)
                            LexType -> return (TokenType p)
                            LexIf -> return (TokenIf p)
                            LexThen -> return (TokenThen p)
                            LexElse -> return (TokenElse p)
                            LexWhile -> return (TokenWhile p)
                            LexDo -> return (TokenDo p)
                            LexOf -> return (TokenOf p)
                            LexLet -> return (TokenLet p)
                            LexIn -> return (TokenIn p)
                            LexAssign -> return (TokenAssign p)
                            LexPlus -> return (TokenPlus p)
                            LexMinus -> return (TokenMinus p)
                            LexTimes -> return (TokenTimes p)
                            LexDiv -> return (TokenDiv p)
                            LexLParen -> return (TokenLParen p)
                            LexRParen -> return (TokenRParen p)
                            LexPipe -> return (TokenPipe p)
                            LexArrow -> return (TokenArrow p)
                            LexComa -> return (TokenComa p)
                            LexEQ -> return (TokenEQ p)
                            LexGT -> return (TokenGT p)
                            LexLT -> return (TokenLT p)
                            LexLTE -> return (TokenLTE p)
                            LexGTE -> return (TokenGTE p)
                            LexSym -> return (TokenSym p t)
                            LexFunctionDef -> return (TokenFunctionDef p (stripFirstWord t))
                            LexInt -> return ( TokenInt p ((read t) :: Int))
                            LexBool -> return ( TokenBool p (if t == "true" then True else False))


-- No idea why I have to write this myself. Documentation doesn't mention it.
alexEOF :: Alex Token
alexEOF = return TokenEOF

data LexClass = LexComment
              | LexType
              | LexIf
              | LexThen
              | LexElse
              | LexWhile
              | LexDo
              | LexOf
              | LexLet
              | LexIn
              | LexAssign
              | LexPlus
              | LexMinus
              | LexTimes
              | LexDiv
              | LexLParen
              | LexRParen
              | LexPipe
              | LexArrow
              | LexComa
              | LexEQ
              | LexGT
              | LexLT
              | LexLTE
              | LexGTE
              | LexSym
              | LexFunctionDef
              | LexInt
              | LexBool
              | LexEOF
              deriving (Eq,Show)

lexwrap = (alexMonadScan >>=)

}
