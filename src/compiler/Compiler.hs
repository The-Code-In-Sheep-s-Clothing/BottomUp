module Compiler where
import Ast
import Tokens

compile :: [Stmt] -> String
compile x = compile_loop x ++ "main = return()"
compile_loop [] = ""
compile_loop (x:xs) = compile_stmt x ++ "\n" ++ compile_loop xs

compile_stmt :: Stmt -> String
compile_stmt (Typedef s t) = compile_typedef (Typedef s t)
compile_stmt (TypedefFunc s e t ) = compile_typedef (TypedefFunc s e t)
compile_stmt (Valdef s e) = compile_valdef (Valdef s e)
compile_stmt (SExpr e) = compile_expr e
compile_stmt (Conditional e1 e2 e3) = 
    "if " ++ compile_expr e1 ++ "then" ++ compile_expr e2 ++ "else" ++ compile_expr e3

compile_valdef :: Stmt -> String
compile_valdef (Valdef (Signature s t) e) = 
    s ++ " :: " ++ compile_type t ++ "\n" ++ valdef_loop e
valdef_loop [] = ""
valdef_loop (e:es) = compile_equation e ++ "\n" ++ valdef_loop es

compile_equation :: Equation -> String
compile_equation (Equation s e st) = s ++ " " ++ equation_loop e ++ "=" ++ compile_stmt st
equation_loop [] = ""
equation_loop (e:es) = "(" ++ compile_expr e ++ ")" ++ equation_loop es

compile_typedef :: Stmt -> String
compile_typedef (TypedefFunc "Board" e t) = 
    "data Board = Grid Int Int\n" ++
    "data Tile_values = " ++
    compile_type t ++ "\n" ++
    "board = " ++
    compile_expr e
compile_typedef (Typedef "Input" t) = 
    "type Position = (Int, Int)\n" ++ 
    "type Input = " ++ 
    compile_type t

compile_type :: Type -> String
compile_type (Ptype' p) = compile_ptype p
compile_type (Ftype' f) = compile_ftype f

compile_ptype :: Ptype -> String
compile_ptype (Xtype' x) = compile_xtype x
compile_ptype (Ttype' t) = compile_ttype t

compile_ftype :: Ftype -> String
compile_ftype (Ftype p1 p2) = compile_ptype p1 ++ "->" ++ compile_ptype p2

compile_xtype :: Xtype -> String
compile_xtype (Xtype (x:xs)) =
    if null xs then
        compile_btype x
    else
        compile_btype x ++ "|" ++ compile_xtype (Xtype xs)

compile_ttype :: Ttype -> String
compile_ttype (Ttype x) = "(" ++ loop_ttype x ++ ")"
loop_ttype (x:xs) =
    if null xs then
        compile_xtype x
    else
        compile_xtype x ++ "," ++ loop_ttype xs

compile_btype :: Btype -> String
compile_btype (Btype b) = b

compile_expr :: Expr -> String
compile_expr (EInt i) = show i
compile_expr (ESymbol s) = s
compile_expr (Paren e) = "(" ++ compile_expr e ++ ")"
compile_expr (Tuple t) = "(" ++ loop_expr_tuple t ++ ")"
compile_expr (FunctionApp s e) = s ++ " " ++ loop_expr_func e
compile_expr (Infix e1 b e2) = compile_expr e1 ++ compile_binop b ++ compile_expr e2
compile_expr (Empty) = ""
loop_expr_tuple (e:es) = 
    if null es then
        compile_expr e
    else
        compile_expr e ++ "," ++ loop_expr_tuple es
loop_expr_func (e:es) =
    if null es then
        compile_expr e
    else
        compile_expr e ++ " " ++ loop_expr_func es

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
