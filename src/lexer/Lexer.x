{
module Lexer where

import Data.List.Split
import System.Exit
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
$capital = [A-Z]
$lower = [a-z]
$alphaNumeric = [$alpha $digit \_ \']

-- text converted to lexer tokens
tokens :-
  $white+                 { mkL LexWhite }
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
  "->"                    { mkL LexArrow }
  ","                     { mkL LexComa }
  "=="                    { mkL LexEQ }
  ">"                     { mkL LexGT }
  "<"                     { mkL LexLT }
  "=<"                    { mkL LexLTE }
  "=>"                    { mkL LexGTE }
  "!"                     { mkL LexBang }
  "{"                     { mkL LexLCurly }
  "}"                     { mkL LexRCurly }
  ":"                     { mkL LexColon }
  "&"                     { mkL LexAmp }
  $lower $alphaNumeric*   { mkL LexSym }
  $capital $alphaNumeric* { mkL LexTypeName }
  $digit+                 { mkL LexInt }
  True                    { mkL LexBool }
  Frue                    { mkL LexBool }
  .                       { mkL LexError }

{

data AlexUserState = AlexUserState
                   {
                      readInput :: String,
                      nextToken :: Token,
                      prevState :: (AlexPosn, String, String)
                   }

alexInitUserState :: AlexUserState
alexInitUserState  = AlexUserState { readInput = "", nextToken = TokenSOF, prevState = ((AlexPn 0 0 0), "", "") }

getLexerReadInputValue :: Alex String
getLexerReadInputValue  = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, readInput ust)

addLexerReadInputValue    :: String -> Alex ()
addLexerReadInputValue ss  = Alex $ \s -> Right (s{alex_ust=(alex_ust s){readInput=readInput (alex_ust s) ++ ss}}, ())

getNextToken :: Alex Token
getNextToken = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, nextToken ust)

setNextToken :: Token -> Alex ()
setNextToken ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){nextToken=ss}}, ())

getPrevState :: Alex (AlexPosn, String, String)
getPrevState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, prevState ust)

setPrevState :: (AlexPosn, String, String) -> Alex ()
setPrevState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){prevState=ss}}, ())


-- there might be a library for this, but I dont know how to include
stripFirstWord          :: String -> String
stripFirstWord (' ':xs) = ""
stripFirstWord (x:xs)   = x : stripFirstWord xs
stripFirstWord _        = ""

-- convert lex token to string for errors
prettyToken                          :: Token -> String
prettyToken ( TokenComment _ _ )     = "Comment"
prettyToken ( TokenType _ )          = "type Keyword"
prettyToken ( TokenIf _ )            = "if Keyword"   
prettyToken ( TokenThen _ )          = "then Keyword"
prettyToken ( TokenElse _ )          = "else Keyword"
prettyToken ( TokenWhile _ )         = "while Keyword"
prettyToken ( TokenDo _ )            = "do Keyword"
prettyToken ( TokenOf _ )            = "of Keyword"
prettyToken ( TokenLet _ )           = "let Keyword"
prettyToken ( TokenIn _ )            = "in Keyword"
prettyToken ( TokenAssign _ )        = "="
prettyToken ( TokenPlus _ )          = "+"
prettyToken ( TokenMinus _ )         = "-"
prettyToken ( TokenTimes _ )         = "*"
prettyToken ( TokenDiv _ )           = "/"
prettyToken ( TokenLParen _ )        = "("
prettyToken ( TokenRParen _ )        = ")"
prettyToken ( TokenArrow _ )         = "->"
prettyToken ( TokenComa _ )          = ","
prettyToken ( TokenEQ _ )            = "=="
prettyToken ( TokenGT _ )            = "<"
prettyToken ( TokenLT _ )            = ">"
prettyToken ( TokenLTE _ )           = ">="
prettyToken ( TokenGTE _ )           = "<="
prettyToken ( TokenSym _ _ )         = "symbol (variable name)"
prettyToken ( TokenTypeName _ _ )    = "type (Type name)"
prettyToken ( TokenFunctionDef _ _ ) = "Function Definition"
prettyToken ( TokenInt _  _ )        = "Integer"
prettyToken ( TokenBool _ _ )        = "Boolean"
prettyToken ( TokenEOF )             = "End of File"
prettyToken ( TokenError _ _ )       = "Lexical Error"
prettyToken ( TokenWhite _ _ )       = "Whitespace"
prettyToken ( TokenBang _ )          = "!"
prettyToken ( TokenLCurly _ )        = "{"
prettyToken ( TokenRCurly _ )        = "}"
prettyToken ( TokenColon _ )         = ":"
prettyToken ( TokenAmp _ )           = "&"
prettyToken ( TokenSOF )             = "Start of File"

-- The token type:
data Token    = TokenComment      { tokPosition :: AlexPosn, comment :: String }
              | TokenType         { tokPosition :: AlexPosn }
              | TokenIf           { tokPosition :: AlexPosn }
              | TokenThen         { tokPosition :: AlexPosn }
              | TokenElse         { tokPosition :: AlexPosn }
              | TokenWhile        { tokPosition :: AlexPosn }
              | TokenDo           { tokPosition :: AlexPosn }
              | TokenOf           { tokPosition :: AlexPosn }
              | TokenLet          { tokPosition :: AlexPosn }
              | TokenIn           { tokPosition :: AlexPosn }
              | TokenAssign       { tokPosition :: AlexPosn }
              | TokenPlus         { tokPosition :: AlexPosn }
              | TokenMinus        { tokPosition :: AlexPosn }
              | TokenTimes        { tokPosition :: AlexPosn }
              | TokenDiv          { tokPosition :: AlexPosn }
              | TokenLParen       { tokPosition :: AlexPosn }
              | TokenRParen       { tokPosition :: AlexPosn }
              | TokenArrow        { tokPosition :: AlexPosn }
              | TokenComa         { tokPosition :: AlexPosn }
              | TokenEQ           { tokPosition :: AlexPosn }
              | TokenGT           { tokPosition :: AlexPosn }
              | TokenLT           { tokPosition :: AlexPosn }
              | TokenLTE          { tokPosition :: AlexPosn }
              | TokenGTE          { tokPosition :: AlexPosn }
              | TokenSym          { tokPosition :: AlexPosn, name :: String }
              | TokenTypeName     { tokPosition :: AlexPosn, name :: String }
              | TokenFunctionDef  { tokPosition :: AlexPosn, name :: String }
              | TokenInt          { tokPosition :: AlexPosn, int :: Int }
              | TokenBool         { tokPosition :: AlexPosn, bool :: Bool }
              | TokenEOF 
              | TokenSOF
              | TokenError        { tokPosition :: AlexPosn, text :: String }
              | TokenWhite        { tokPosition :: AlexPosn, text :: String }
              | TokenBang         { tokPosition :: AlexPosn }
              | TokenLCurly       { tokPosition :: AlexPosn }
              | TokenRCurly       { tokPosition :: AlexPosn }
              | TokenColon        { tokPosition :: AlexPosn }
              | TokenAmp          { tokPosition :: AlexPosn }
              deriving (Eq,Show)

-- convert lex token to parse token
mkL :: LexClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, _, str) len = let t = take len str
                          in do
                            addLexerReadInputValue t
                            case c of
                              LexComment      -> return ( TokenComment p t )
                              LexType         -> return ( TokenType p )
                              LexIf           -> return ( TokenIf p )
                              LexThen         -> return ( TokenThen p )
                              LexElse         -> return ( TokenElse p )
                              LexWhile        -> return ( TokenWhile p )
                              LexDo           -> return ( TokenDo p )
                              LexOf           -> return ( TokenOf p )
                              LexLet          -> return ( TokenLet p )
                              LexIn           -> return ( TokenIn p )
                              LexAssign       -> return ( TokenAssign p )
                              LexPlus         -> return ( TokenPlus p )
                              LexMinus        -> return ( TokenMinus p )
                              LexTimes        -> return ( TokenTimes p )
                              LexDiv          -> return ( TokenDiv p )
                              LexLParen       -> return ( TokenLParen p )
                              LexRParen       -> return ( TokenRParen p )
                              LexArrow        -> return ( TokenArrow p )
                              LexComa         -> return ( TokenComa p )
                              LexEQ           -> return ( TokenEQ p )
                              LexGT           -> return ( TokenGT p )
                              LexLT           -> return ( TokenLT p )
                              LexLTE          -> return ( TokenLTE p )
                              LexGTE          -> return ( TokenGTE p )
                              LexSym          -> return ( TokenSym p t )
                              LexTypeName     -> return ( TokenTypeName p t )
                              LexFunctionDef  -> return ( TokenFunctionDef p (stripFirstWord t) )
                              LexInt          -> return ( TokenInt p ((read t) :: Int) )
                              LexBool         -> return ( TokenBool p (if t == "true" then True else False) )
                              LexError        -> return ( TokenError p t )
                              LexWhite        -> return ( TokenWhite p t )
                              LexBang         -> return ( TokenBang p )
                              LexLCurly       -> return ( TokenLCurly p )
                              LexRCurly       -> return ( TokenRCurly p )
                              LexColon        -> return ( TokenColon p )
                              LexAmp          -> return ( TokenAmp p )


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
              | LexArrow
              | LexComa
              | LexEQ
              | LexGT
              | LexLT
              | LexLTE
              | LexGTE
              | LexSym
              | LexTypeName
              | LexFunctionDef
              | LexInt
              | LexBool
              | LexEOF
              | LexError
              | LexWhite
              | LexBang
              | LexLCurly
              | LexRCurly
              | LexColon
              | LexAmp
              deriving (Eq,Show)

data LexerError = LexerError String
instance Show LexerError where
  show (LexerError str) = str

showPosn ::  AlexPosn -> String
showPosn (AlexPn _ line col) = "Error on line: " ++ show line ++ " column: " ++  show col ++ "\n"

printError :: String -> Alex a
printError s = do
  ((AlexPn a line col), rem_in, read_in) <- getPrevState
  alexError $ "\n" ++ s ++ showPosn (AlexPn a line col) ++ splitOn "\n" (read_in ++ rem_in) !! (line - 1) ++ "\n" ++ (replicate (col - 2) ' ') ++ "^"

lexNextToken :: Alex Token
lexNextToken = do
  next <- alexMonadScan
  setNextToken next
  case next of
    TokenError posn text -> printError $ "unexpected character " ++ text ++ "\n"
    TokenWhite _ _ -> lexNextToken
    _ -> getNextToken

updatePrevState :: (Token -> Alex a) -> Token -> (AlexPosn, String, String) -> Alex a
updatePrevState cont token state = do
  setPrevState state
  cont token

-- makes this LR2
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap cont = do
    (pos,_, _, rem_in) <- alexGetInput
    read_in <- getLexerReadInputValue
    token <- getNextToken
    next <- lexNextToken
    case token of
      -- special case for start of file
      TokenSOF -> lexwrap cont
      -- fix special case for function starts
      TokenSym posn name -> case next of 
        TokenColon _ -> updatePrevState cont (TokenFunctionDef posn name) (pos, rem_in, read_in)
        _ -> updatePrevState cont (TokenSym posn name) (pos, rem_in, read_in)
      _ -> updatePrevState cont token (pos, rem_in, read_in)

}
