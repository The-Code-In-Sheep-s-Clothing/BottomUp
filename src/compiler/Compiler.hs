module Compiler where
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Ast

type Env = Map String String

compile :: [Stmt] -> String
compile x = "import Builtins\n" ++ "data Outcome = P Player | Tie deriving Show\n" ++ compile_loop x ++ 
    "main = print $ result"
compile_loop [] = ""
compile_loop (x:xs) = compile_stmt x ++ "\n" ++ compile_loop xs

compile_stmt :: Stmt -> String
compile_stmt (Typedef s t) = compile_typedef (Typedef s t)
compile_stmt (TypedefFunc s e t ) = compile_typedef (TypedefFunc s e t)
compile_stmt (Valdef s e) = compile_valdef (Valdef s e) ++ "\n"
compile_stmt (SExpr e) = compile_expr e
compile_stmt (Let s e st) = "let " ++ s ++ "=" ++ compile_expr e ++ " in " ++ compile_stmt st
compile_stmt (Conditional e s1 s2) = 
    "if " ++ compile_expr e ++ " then " ++ compile_stmt s1 ++ " else " ++ compile_stmt s2
compile_stmt (While (FunctionApp f1 e1) (FunctionApp f2 e2)) =
    "while " ++ "(" ++ f1 ++ while_compose e1 ++ ")" ++ " " ++ 
    "(" ++ f2  ++ ")"++ " " ++ compile_expr e2 
while_compose (FunctionApp f e) = "." ++ f ++ while_compose e
while_compose _ = ""

-- Function type definition
-- TODO accept different types of arguments for initialBoard
compile_valdef :: Stmt -> String
compile_valdef (Valdef (Signature "initialBoard" t) ((Equation s e st):es)) =
    s ++ " :: Grid -> " ++ compile_type t ++ "\n" ++
    "initialBoard (Grid (x,y)) = board (x, y)" ++ compile_stmt st
-- Hardcoding this for now (TODO)
compile_valdef (Valdef (Signature "outcome" t) e) = 
    "outcome" ++ " :: " ++ compile_type t ++ "\n" ++ intercalate "\n" (map compile_equation_outcome e)
compile_valdef (Valdef (Signature s t) e) = 
    s ++ " :: " ++ compile_type t ++ "\n" ++ intercalate "\n" (map compile_equation e)

-- Function defintion equations
compile_equation :: Equation -> String
compile_equation (Equation s e st) = s ++ " " ++ compile_expr e ++ "=" ++ compile_stmt st

-- TODO remove
compile_equation_outcome :: Equation -> String
compile_equation_outcome (Equation s e st) = s ++ " " ++ 
    compile_expr e ++ "=" ++ compile_stmt_outcome st

-- TODO remove
compile_stmt_outcome :: Stmt -> String
compile_stmt_outcome (SExpr (ESymbol "A")) = "P A"
compile_stmt_outcome (SExpr (ESymbol "B")) = "P B"
compile_stmt_outcome (Conditional e s1 s2) = 
    "if " ++ compile_expr e ++ " then " ++ compile_stmt_outcome s1 ++ 
    " else " ++ compile_stmt_outcome s2
compile_stmt_outcome a = compile_stmt a

-- Type/data declarations (bo/equationard, input)
compile_typedef :: Stmt -> String
compile_typedef (TypedefFunc "Board" e t) = 
    -- "data Content = " ++
    -- compile_type t ++ "\n" ++
    "board_size = " ++
    compile_expr e
compile_typedef (Typedef "Input" t) = 
    "type Input = " ++ 
    compile_type t
compile_typedef (Typedef s t) = "type " ++ s ++ " = " ++ compile_type t

compile_type :: Type -> String
compile_type (Ptype' p) = compile_ptype p
compile_type (Ftype' f) = compile_ftype f

compile_ptype :: Ptype -> String
compile_ptype (Xtype' x) = compile_xtype x
compile_ptype (Ttype' t) = compile_ttype t

compile_ftype :: Ftype -> String
compile_ftype (Ftype p1 p2) = compile_ptype p1 ++ "->" ++ compile_ptype p2

compile_xtype :: Xtype -> String
compile_xtype (Xtype [Btype "Player", Btype "Tie"]) = "Outcome"
compile_xtype (Xtype b) = intercalate "|" (map compile_btype b)

compile_ttype :: Ttype -> String
compile_ttype (Ttype x) = "(" ++ loop_ttype x ++ ")"
loop_ttype x = intercalate "," (map compile_xtype x)

compile_btype :: Btype -> String
compile_btype (Btype b) = b

compile_expr :: Expr -> String
compile_expr (EInt i) = show i
compile_expr (ESymbol s) 
    | s == "initialBoard" = "initialBoard board_size" 
    | s == "input" = "input []"
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
