module Compiler where
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Data.Strings
import Ast
import Builtins
import Data.Array

-- List of type names and their extended type symbols
type Env = [(String, String, [String])]
type StateRet = State Env String

-- Append string to functor
(<++) :: (Functor f) => f String -> String -> f String 
x <++ s = (++s) <$> x

(++>) :: (Functor f) => String -> f String -> f String 
s ++> x = (s++) <$> x

(<++>) :: (Applicative f) => f String -> f String -> f String 
x1 <++> x2 = (++) <$> x1 <*> x2

intercalateM :: (Monad m) => String -> [m String] -> m String
intercalateM s l = sequence l >>= \x -> return (intercalate s x)

-- Helper functions to compile all the builtins
compile_imports :: String
compile_imports = "-- Imports section\n" ++ (intercalate "\n" imports) ++ "\n\n"

compile_builtin_types :: String
compile_builtin_types = "-- Builtin types\n" ++ (intercalate "\n" builtin_types) ++ "\n\n"

compile_builtin_funcs :: Env -> String
compile_builtin_funcs st = "-- Builtin functions\n" ++ (intercalate "\n\n" builtin_funcs)

-- Compilation code from AST
compile :: [Stmt] -> (String, Env)
compile x = let (res, st) = runState (compile_stmts x) [] in 
    ("import OutputBuiltins\nimport Data.Array\nimport System.IO.Unsafe" ++ compile_state st ++ "\n" ++ res, st)

compile_prelude :: [Stmt] -> Env -> String
compile_prelude x e = let (res, st) = runState (compile_stmts x) e in compile_state_no_dup st e  ++ compile_board_size st ++ "\n" ++ res

compile_builtin :: [Stmt] -> String
compile_builtin x = "module OutputBuiltins where\n" ++ compile_imports ++ compile_builtin_types ++
    let (res, st) = runState (compile_stmts x) [] in
        compile_content_state st ++ "\n" ++
        compile_builtin_funcs st ++ "\n\n" ++
        find_compile_input_funcs x

find_compile_input_funcs :: [Stmt] -> String
find_compile_input_funcs [] = "" 
find_compile_input_funcs ((Typedef "Input" t _):xs) = let (res, st) = runState (compile_type t) [] in
    "type Input = " ++ res ++ "\n" ++ compile_input_funcs2 t
find_compile_input_funcs (x:xs) = find_compile_input_funcs xs

compile_state :: Env -> String
compile_state e = intercalate "\n" (map compile_single_state e) ++ "\n"

compile_state_no_dup :: Env -> Env -> String
compile_state_no_dup st1 st2 = intercalate "\n" (map compile_single_state (st1 \\ st2)) ++ "\n"

compile_board_size :: Env -> String
compile_board_size ((s,c,l):es) = if (s == "board_size") then
    "board_size = " ++ c ++ "\n"
    else
        compile_board_size es

compile_content_state :: Env -> String
compile_content_state ((s,c,l):es) = if (s == "Content") then
    "data " ++ s ++ " = " ++ s ++ "Con " ++ 
    c ++ "|" ++ intercalate " | " l ++
    " deriving (Show, Eq)"
    else
        compile_content_state es

compile_single_state :: (String, String, [String]) -> String
compile_single_state ("Content", c, l) = ""
compile_single_state ("board_size", c, l) = ""
compile_single_state (s, c, l) = "data " ++ s ++ " = " ++ s ++ "Con " ++ 
    c ++ "|" ++ intercalate " | " l ++
    " deriving (Show, Eq)"

compile_stmts :: [Stmt] -> StateRet
compile_stmts [] = return ""
compile_stmts (x:xs) = (++) <$> (compile_stmt "" x <++ "\n") <*> compile_stmts xs

compile_stmt :: String -> Stmt -> StateRet
compile_stmt f (SExpr (ESymbol e _) _) = do
    cur_state <- get
    if elem f (map (\(a,_,_) -> a) cur_state) then
        if elem e (get_state_xtypes f cur_state) then
            return (e)
        else
            return (f ++ "Con " ++ e)
    else
        return (e)
compile_stmt f (SExpr e _) = compile_expr_state e
compile_stmt _ (Typedef s t p) = compile_typedef (Typedef s t p)
compile_stmt _ (TypedefFunc s e t p) = compile_typedef (TypedefFunc s e t p)
compile_stmt _ (Valdef s e p) = compile_valdef (Valdef s e p) <++ "\n"
compile_stmt f (Let s e st _) = ("let " ++ s ++ "=" ++ compile_expr e ++ " in ") ++> compile_stmt f st
compile_stmt f (Conditional e s1 s2 _) = 
    ("if " ++ compile_expr e ++ " then ") ++> compile_stmt f s1 <++> (" else " ++> compile_stmt f s2)
compile_stmt _ (While (FunctionApp f1 e1 _) (FunctionApp f2 e2 _) _) = return (
    "while " ++ "(" ++ f1 ++ while_compose e1 ++ ")" ++ " " ++ 
    "(" ++ f2  ++ ")" ++ " " ++ compile_expr e2 )
compile_stmt _ (SComment s _) = return s
while_compose (FunctionApp f e _) = "." ++ f ++ while_compose e
while_compose _ = ""

-- Function type definition
compile_valdef :: Stmt -> StateRet
compile_valdef (Valdef (Signature name t _) ((ArrayEquation s e st _):es) _) =
    ((name ++ " :: " ) ++> compile_type t ) <++ ("\n" ++
    s ++ " =") <++> (compile_boardequ es) <++ "board (gridSize board_size) " <++> (compile_stmt "" st) <++ (parenList es)
compile_valdef (Valdef (Signature s t _) e _) = 
    (s ++ " :: ") ++> compile_type t <++ "\n" <++> (intercalateM "\n" (map (compile_equation (get_return_type t)) e))

compile_boardequ :: [Equation] -> StateRet
compile_boardequ [] = return "unsafePerformIO $ printBoardIO $ "
compile_boardequ ((ArrayEquation s (ETuple (TupleList ((TupleValue (ESymbol _ _) _):(TupleValue (EInt y _) _):xs) _) _) st _):es) =
         (compile_boardequ es) <++ ("modifyCol " ++ show(y) ++ " ") <++> (compile_stmt "" st) <++ " ("
compile_boardequ ((ArrayEquation s (ETuple (TupleList ((TupleValue (EInt x _) _):(TupleValue (ESymbol _ _) _):xs) _) _) st _):es) =
         (compile_boardequ es) <++ ("modifyRow " ++ show(x) ++ " ") <++> (compile_stmt "" st) <++ " ("
compile_boardequ ((ArrayEquation s (ETuple (TupleList ((TupleValue (EInt x _) _):(TupleValue (EInt y _) _):xs) _) _) st _):es) =
         (compile_boardequ es) <++ ("modifyElement (" ++ show(x) ++ ", " ++ show(y) ++ ") ") <++> ("(" ++> (compile_stmt "Content" st) <++ ")") <++ " ("

parenList :: [Equation] -> String
parenList [] = ""
parenList (x:xs) = ")" ++ parenList xs

-- Function defintion equations
compile_equation :: String -> Equation -> StateRet
compile_equation f (Equation s e st _) = (s ++ " " ++ compile_expr e ++ "=") ++> compile_stmt f st
compile_equation f (ArrayEquation s e st _) = (s ++ " " ++ compile_expr e ++ "=") ++> compile_stmt f st -- this probably does not work as intneded

-- Type/data declarations (bo/equationard, input)
compile_typedef :: Stmt -> StateRet
compile_typedef (TypedefFunc "Board" e t _) = 
    add_content_to_state t >> add_board_size_to_state e >> return ""
-- compile_typedef (Typedef "Input" t _) = "type Input = " ++> compile_type t <++ "\n"
compile_typedef (Typedef "Input" t _) = return ""
compile_typedef (Typedef s (Ptype' (Ttype' t _) _) _) = 
    ("type " ++ s ++ " = ") ++> compile_ttype t
compile_typedef (Typedef s (Ptype' (Xtype' (Xtype b [] _) _) _) _) = 
    return (("type " ++ s ++ " = ") ++ compile_btype b)
compile_typedef (Typedef s (Ptype' (Xtype' (Etype l ep) _) _) _) = 
    ("data " ++ s ++ " = ") ++> (compile_xtype (Etype l ep)) <++ " deriving (Eq, Show)"
compile_typedef (Typedef s (Ptype' (Xtype' x@(Xtype b l _) _) _) _) = 
    compile_xtype_named x s >> return ""
-- This is impossible, all possible cases are captured above
compile_typedef _ = return ""

compile_input_funcs2 :: Type -> String
compile_input_funcs2 t = "-- Input functions\n" ++ (intercalate "\n\n" input_funcs) ++ "\n\n" ++
    "check_input :: GenParser Char st Input\n" ++
    "check_input = do\n" ++
    "    char \'(\'\n" ++
    read_int_tuple ((count_tuple_type t) - 1) ++
    "    s0 <- many digit\n" ++
    "    char \')\'\n" ++
    "    return (" ++ return_int_tuple ((count_tuple_type t) - 1) ++ "read s0::Int)"

read_int_tuple :: Int -> String
read_int_tuple 0 = ""
read_int_tuple i = "    s" ++ show i ++ " <- " ++ "many digit\n    char \',\'\n" ++ (read_int_tuple $ i-1)

return_int_tuple :: Int -> String
return_int_tuple 0 = ""
return_int_tuple i = "read s" ++ show i ++ "::Int,"

count_tuple_type :: Type -> Int
count_tuple_type (Ptype' (Xtype' (Xtype (Btype "Position" _) [] _) _) _) = 2
count_tuple_type (Ptype' (Xtype' (Xtype b [] _) _) _) = 1
count_tuple_type (Ptype' (Ttype' (Ttype t _) _) _) = length t --this doesnt really work for nested tuples

compile_type :: Type -> StateRet
compile_type (Ptype' p _) = compile_ptype p
compile_type (Ftype' f _) = compile_ftype f

compile_ptype :: Ptype -> StateRet
compile_ptype (Xtype' x _) = compile_xtype x
compile_ptype (Ttype' t _) = compile_ttype t

compile_ftype :: Ftype -> StateRet
compile_ftype (Ftype p1 p2 _) = compile_ptype p1 <++ " -> " <++> compile_ptype p2

compile_xtype :: Xtype -> StateRet
compile_xtype (Xtype b [] _) = return (compile_btype b)
compile_xtype (Xtype b l _) = do
    cur_state <- get
    let type_list = [compile_btype b] ++ l
    let t = intercalate "_" type_list
    if not (elem t (map (\(a,_,_) -> a) cur_state)) then do
        put (cur_state ++ [(t, compile_btype b, drop 1 type_list)])
        return t
    else do
        return t
compile_xtype (Etype l _) = return (intercalate "|" l)

compile_xtype_named :: Xtype -> String -> StateRet
compile_xtype_named (Xtype b [] _) s = return (compile_btype b)
compile_xtype_named (Xtype b l _) s= do
    cur_state <- get
    let type_list = [compile_btype b] ++ l
    let t = intercalate "_" type_list
    if not (elem t (map (\(a,_,_) -> a) cur_state)) then do
        put (cur_state ++ [(s, compile_btype b, drop 1 type_list)])
        return t
    else do
        return t
compile_xtype_named (Etype l _) s = return (intercalate "|" l)

add_content_to_state :: Type -> StateRet
add_content_to_state (Ptype' (Xtype' (Xtype b l _) _) _) = do
    cur_state <- get
    let type_list = [compile_btype b] ++ l
    let t = "Content"
    if not (elem t (map (\(a,_,_) -> a) cur_state)) then do
        put (cur_state ++ [("Content", (compile_btype b), drop 1 type_list)])
        return ""
    else do
        return ""
-- TODO: Should this be possible?
add_content_to_state _ = return "Not Possible"

add_board_size_to_state :: Expr -> StateRet
add_board_size_to_state e = do
    cur_state <- get
    put (cur_state ++ [("board_size", compile_expr e, [])])
    return ""

compile_ttype :: Ttype -> StateRet
compile_ttype (Ttype x _) = "(" ++> (intercalateM "," (map compile_ptype x)) <++ ")"

compile_btype :: Btype -> String
-- compile_btype (Btype "Board") = "Board Content"
compile_btype (Btype b _) = b

compile_tuple :: Tuple -> String
compile_tuple (TupleList t _) = "(" ++ intercalate "," (map compile_tuple t) ++ ")"
compile_tuple (TupleValue t _) = compile_expr t

compile_tuple_state :: Tuple -> StateRet
compile_tuple_state (TupleList t _) = "(" ++> (intercalateM "," (map compile_tuple_state t)) <++ ")"
compile_tuple_state (TupleValue t _) = compile_expr_state t

compile_expr :: Expr -> String
compile_expr (EInt i _) = show i
compile_expr (ESymbol s _)
    | s == "input" = "input 0"
    | otherwise = s
compile_expr (Paren e _) = "(" ++ compile_expr e ++ ")"
compile_expr (ETuple t _) = compile_tuple t
-- Need to check if it's a built in function with a different signature (or,and,...)
compile_expr (FunctionApp s t@(ETuple e@(TupleList l _) _) _) 
    | s == "or" || s == "and" = "(" ++ s ++ " [" ++ 
    intercalate "," (map compile_tuple l) ++ "])" -- this probably doesnt work the way you want it to
    | otherwise = "(" ++ s ++ " " ++ compile_expr t ++ ")"
compile_expr (FunctionApp s e _)
    | s == "or" || s == "and" = "(" ++ s ++ " [" ++ 
    compile_expr e ++ "])"
    | otherwise = "(" ++ s ++ " " ++ compile_expr e ++ ")"
compile_expr (Infix e1 b e2 _) = compile_expr e1 ++ compile_binop b ++ compile_expr e2
compile_expr (Empty) = ""
loop_expr_func (e:es) =
    if null es then
        "(" ++ compile_expr e ++ ")"
    else
        "(" ++ compile_expr e ++ ") " ++ loop_expr_func es

compile_expr_state :: Expr -> StateRet
compile_expr_state (ETuple t _) = compile_tuple_state t
compile_expr_state eo@(FunctionApp s t@(ETuple e@(TupleList l x1) x2) x3) 
    | s == "place" =  do
        cur_state <- get
        let f = "Content"
        if elem f (map (\(a,_,_) -> a) cur_state) then
            if elem (compile_tuple (head l)) (get_state_xtypes f cur_state) then
                return (compile_expr eo)
            else
                let new_list = ((TupleValue (ESymbol (f ++ "Con " ++ compile_tuple (head l)) x1) x1) : (tail l)) in
                    return (compile_expr (FunctionApp s (ETuple (TupleList new_list x1) x2) x3))
        else
            return (compile_expr eo)
    | otherwise = return (compile_expr eo)
compile_expr_state (Infix e1 b e2 _) = compile_expr_state_binop e1 <++> ((compile_binop b) ++> compile_expr_state_binop e2)
compile_expr_state e = return (compile_expr e)

compile_expr_state_binop :: Expr -> StateRet
compile_expr_state_binop (ESymbol e _) = do
    cur_state <- get
    let f = "Content"
    if elem f (map (\(a,_,_) -> a) cur_state) then
        if elem e (get_state_xtypes f cur_state) then
            return (e)
        else
            return (f ++ "Con " ++ e)
    else
        return (e)
compile_expr_state_binop e = return (compile_expr e)

compile_binop :: Binop -> String
compile_binop (Plus _ ) = "+"
compile_binop (Minus _ ) = "-"
compile_binop (Times _ ) = "*"
compile_binop (Div _ ) = "/"
compile_binop (EqualTo _ ) = "=="
compile_binop (LessThan _ ) = "<"
compile_binop (GreaterThan _ ) = ">"
compile_binop (LessThanEqual _ ) = "<="
compile_binop (GreaterThanEqual _ ) = ">="
compile_binop (Bang _) = "!"

get_return_type :: Type -> String
get_return_type (Ptype' p _) = evalState (compile_ptype p) []
get_return_type (Ftype' (Ftype _ p2 _) _) = evalState (compile_ptype p2) []

get_state_xtypes :: String -> Env -> [String]
get_state_xtypes s [] = []
get_state_xtypes s (e:es) = let (f,sd,t) = e in 
    if f == s then t else get_state_xtypes s es
