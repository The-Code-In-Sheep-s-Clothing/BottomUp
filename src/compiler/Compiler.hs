module Compiler where
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Ast

type Env = [(String, [String])]
type StateRet = State Env String

-- Helpers
-- Append string to functor
(<++) :: (Functor f) => f String -> String -> f String 
x <++ s = (++s) <$> x

(++>) :: (Functor f) => String -> f String -> f String 
s ++> x = (s++) <$> x

(<++>) :: (Applicative f) => f String -> f String -> f String 
x1 <++> x2 = (++) <$> x1 <*> x2

intercalateM :: (Monad m) => String -> [m String] -> m String
intercalateM s l = sequence l >>= \x -> return (intercalate s x)


-- Compilation Code
compile :: [Stmt] -> String
compile x = "import Builtins\n" ++ 
    let (res, st) = runState (compile_stmts x) [] in
        compile_state st ++ "\n" ++ res

compile_state :: Env -> String
compile_state e = intercalate "\n" (map compile_single_state e)

compile_single_state :: (String, [String]) -> String
compile_single_state (s, l) = "data " ++ s ++ " = " ++ s ++ "Con " ++ head (splitOn "_" s) ++ "|" ++ intercalate " | " l

compile_stmts :: [Stmt] -> StateRet
compile_stmts [] = return ""
compile_stmts (x:xs) = (++) <$> (compile_stmt "" x <++ "\n") <*> compile_stmts xs

compile_stmt :: String -> Stmt -> StateRet
compile_stmt f (SExpr (ESymbol e)) = do
    cur_state <- get
    if elem f (map fst cur_state) then
        if elem e (get_snd_state f cur_state) then
            return (e)
        else
            return (f ++ "Con " ++ e)
    else
        return (e)
compile_stmt f (SExpr e) = return (compile_expr e)
compile_stmt _ (Typedef s t) = compile_typedef (Typedef s t)
compile_stmt _ (TypedefFunc s e t ) = compile_typedef (TypedefFunc s e t)
compile_stmt _ (Valdef s e) = compile_valdef (Valdef s e) <++ "\n"
compile_stmt f (Let s e st) = ("let " ++ s ++ "=" ++ compile_expr e ++ " in ") ++> compile_stmt f st
compile_stmt f (Conditional e s1 s2) = 
    ("if " ++ compile_expr e ++ " then ") ++> compile_stmt f s1 <++> (" else " ++> compile_stmt f s2)
compile_stmt _ (While (FunctionApp f1 e1) (FunctionApp f2 e2)) = return (
    "while " ++ "(" ++ f1 ++ while_compose e1 ++ ")" ++ " " ++ 
    "(" ++ f2  ++ ")" ++ " " ++ compile_expr e2 )
while_compose (FunctionApp f e) = "." ++ f ++ while_compose e
while_compose _ = ""

-- Function type definition
-- TODO accept different types of arguments for initialBoard
compile_valdef :: Stmt -> StateRet
compile_valdef (Valdef (Signature "initialBoard" t) ((Equation s e st):es)) =
    (((s ++ " :: Grid -> ") ++> compile_type t ) <++ ("\n" ++
    "initialBoard (Grid (x,y)) = board (x, y)")) <++> (compile_stmt "" st)
compile_valdef (Valdef (Signature s t) e) = 
    (s ++ " :: ") ++> compile_type t <++ "\n" <++> (intercalateM "\n" (map (compile_equation (get_return_type t)) e))

-- Function defintion equations
compile_equation :: String -> Equation -> StateRet
compile_equation f (Equation s e st) = (s ++ " " ++ compile_expr e ++ "=") ++> compile_stmt f st

-- Type/data declarations (bo/equationard, input)
compile_typedef :: Stmt -> StateRet
compile_typedef (TypedefFunc "Board" e t) = return (
    -- "data Content = " ++
    -- compile_type t ++ "\n" ++
    "board_size = " ++
    compile_expr e)
compile_typedef (Typedef "Input" t) = 
    "type Input = " ++> compile_type t
compile_typedef (Typedef s t) = ("type " ++ s ++ " = ") ++> compile_type t

compile_type :: Type -> StateRet
compile_type (Ptype' p) = compile_ptype p
compile_type (Ftype' f) = compile_ftype f

compile_ptype :: Ptype -> StateRet
compile_ptype (Xtype' x) = compile_xtype x
compile_ptype (Ttype' t) = compile_ttype t

compile_ftype :: Ftype -> StateRet
compile_ftype (Ftype p1 p2) = compile_ptype p1 <++ " -> " <++> compile_ptype p2

compile_xtype :: Xtype -> StateRet
compile_xtype (Xtype b []) = return (compile_btype b)
compile_xtype (Xtype b l) = do
    cur_state <- get
    let type_list = [compile_btype b] ++ l
    let t = intercalate "_" type_list
    if not (elem t (map fst cur_state)) then do
        put (cur_state ++ [(t, drop 1 type_list)])
        return t
    else do
        return t

compile_ttype :: Ttype -> StateRet
compile_ttype (Ttype x) = "(" ++> (intercalateM "," (map compile_xtype x)) <++ ")"

compile_btype :: Btype -> String
compile_btype (Btype b) = b

compile_expr :: Expr -> String
compile_expr (EInt i) = show i
compile_expr (ESymbol s) 
    | s == "initialBoard" = "initialBoard board_size" 
    | otherwise = s
compile_expr (Paren e) = "(" ++ compile_expr e ++ ")"
compile_expr (Tuple t) = "(" ++ intercalate "," (map compile_expr t) ++ ")"
-- Need to check if it's a built in function with a different signature (or,and,...)
compile_expr (FunctionApp s t@(Tuple e)) 
    | s == "or" || s == "and" = "(" ++ s ++ " [" ++ 
    intercalate "," (map compile_expr e) ++ "])"
    | otherwise = "(" ++ s ++ " " ++ compile_expr t ++ ")"
compile_expr (FunctionApp s e)
    | s == "or" || s == "and" = "(" ++ s ++ " [" ++ 
    compile_expr e ++ "])"
    | s == "input" = "(" ++ s ++ " " ++ compile_expr e ++ " [])"
    | otherwise = "(" ++ s ++ " " ++ compile_expr e ++ ")"
compile_expr (Infix e1 b e2) = compile_expr e1 ++ compile_binop b ++ compile_expr e2
compile_expr (Empty) = ""
loop_expr_func (e:es) =
    if null es then
        "(" ++ compile_expr e ++ ")"
    else
        "(" ++ compile_expr e ++ ") " ++ loop_expr_func es

compile_binop :: Binop -> String
compile_binop Plus = "+"
compile_binop Minus = "-"
compile_binop Times = "*"
compile_binop Div = "/"
compile_binop EqualTo = "=="
compile_binop LessThan = "<"
compile_binop GreaterThan = ">"
compile_binop LessThanEqual = "<="
compile_binop GreaterThanEqual = ">="

get_return_type :: Type -> String
get_return_type (Ptype' p) = evalState (compile_ptype p) []
get_return_type (Ftype' (Ftype _ p2)) = evalState (compile_ptype p2) []

get_snd_state :: String -> Env -> [String]
get_snd_state s [] = []
get_snd_state s (e:es) = if fst e == s then snd e else get_snd_state s es