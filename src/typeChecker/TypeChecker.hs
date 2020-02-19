module TypeChecker where
import Ast
import Data.List
import Debug.Trace

data State       = State [Def] [Def]
                 deriving Show

data Def        = Tdef String NewType
                | Fdef String NewType NewType
                 deriving Show

data NewType = Sgl BaseType
             | Dbl [BaseType]
             | Tpl [[BaseType]]
             deriving Show
data BaseType  = Base String 
               | Sls String
               | Ifblk
               | Endif
               | Em
               deriving (Show,Eq)


exists_in_dbl :: [BaseType] -> String -> State -> Bool
exists_in_dbl [] s st = False
exists_in_dbl ((Sls s1):xs) s2 st = if(s1 == s2)
                                        then True
                                        else exists_in_dbl xs s2 st
exists_in_dbl ((Base s1):xs) s2 (State fs ts) = if(s1 == s2) 
                                                        then True
                                                        else case (lookup5 ts s1) of 
                                                                [Tdef s3 (Dbl b)] -> exists_in_dbl (b ++ xs) s2 (State fs ts)
                                                                [Tdef _ (Sgl s)] -> if((bs_to_str s) == s1)
                                                                                        then exists_in_dbl xs s2 (State fs ts)
                                                                                        else exists_in_dbl ([s] ++ xs) s2 (State fs ts)
                                                                _       -> exists_in_dbl xs s2 (State fs ts)
exists_in_dbl (x:xs) s2 st = exists_in_dbl xs s2 st  

if_extract :: [[BaseType]] -> ([[BaseType]], [[BaseType]])
if_extract ([(Endif)]:xs) = ([], xs)
if_extract (x:xs) = let (b1, b2) = if_extract xs
                        in ([x] ++ b1, b2)

sgl_comp :: BaseType -> BaseType -> State -> Bool
sgl_comp Em _ _ = True
sgl_comp _ Em _ = True
sgl_comp (Base s1) (Base s2) st = (peel s1 st) == (peel s2 st)
sgl_comp (Sls s1) (Sls s2) st = s1 == s2
sgl_comp _ _ _ = False 

dbl_comp :: [BaseType] -> [BaseType] -> State -> Bool
dbl_comp (x:xs) [] _ = False
dbl_comp [] _ _ = True
dbl_comp (x1:xs1) xs2 st = if(exists_in_dbl xs2 (peel (bs_to_str x1) st) st)
                                        then dbl_comp xs1 xs2 st
                                        else False  


tpl_comp :: [[BaseType]] -> [[BaseType]] -> State -> Bool
tpl_comp ([Ifblk]:bs1) b2 st = let (bs11, bs12) = if_extract bs1
                                        in (tpl_comp bs11 b2 st) && (tpl_comp bs12 b2 st)
tpl_comp b1 ([Ifblk]:bs2) st = let (bs21, bs22) = if_extract bs2
                                        in (tpl_comp b1 bs21 st) && (tpl_comp b1 bs22 st)                                                        
tpl_comp [d1] [d2] st = dbl_comp d1 d2 st
tpl_comp (d1:ds1) [d2] st = False
tpl_comp [d1] (d2:ds2) st = False
tpl_comp (d1:ds1) (d2:ds2) st = if(dbl_comp d1 d2 st)
                                        then tpl_comp ds1 ds2 st 
                                        else False

b_d :: BaseType -> State -> [BaseType]
b_d (Base s) (State fs ts) = let d = lookup5 ts s
                                 in if(null d)
                                        then [Base s] 
                                        else case d of
                                                [Tdef _ (Dbl d2)] -> d2
                                                [Tdef _ (Sgl s2)] -> [s2]
                                                _                 -> [Base s]
b_d (Sls s) (State fs ts) = [Sls s]
b_d _ _ = []

b_t :: BaseType -> State -> [[BaseType]]
b_t (Base s) (State fs ts) = let d = lookup5 ts s
                                in if(null d)
                                        then [[Base s]]
                                        else case d of
                                                [Tdef _ (Tpl t)] -> t
                                                [Tdef _ (Sgl s2)] -> [[s2]]
                                                [Tdef _ (Dbl d2)] -> [d2]
                                                _                 -> [[Base s]]


type_comp :: NewType -> NewType -> State -> Bool
type_comp _ (Sgl Em) _ = True
type_comp (Sgl Em) _  _ = True
type_comp (Sgl (Base s1)) (Sgl (Base s2)) st = (peel s1 st) == (peel s2 st)
type_comp (Sgl (Base s1)) (Dbl d) st = dbl_comp (b_d (Base s1) st) d st
type_comp (Sgl (Base s)) (Tpl t) st = tpl_comp (b_t (Base s) st) t st
type_comp (Sgl (Sls s1)) (Sgl s2) st = exists_in_dbl [s2] s1 st
type_comp (Sgl (Sls s1)) (Dbl d) st = exists_in_dbl d s1 st
type_comp (Dbl d1) (Dbl d2) st = dbl_comp d1 d2 st
type_comp (Dbl d1) (Sgl (Base s)) st = dbl_comp d1 (b_d (Base s) st) st
type_comp (Tpl t1) (Tpl t2) st = tpl_comp t1 t2 st
type_comp (Tpl t1) (Sgl (Base s)) st = tpl_comp t1 (b_t (Base s) st) st
type_comp _ _ _ = False

ftype_convert :: String -> Type -> Def
ftype_convert s (Ftype' (Ftype p1 p2 p3) _ ) = Fdef s (type_convert (Ptype' p1 p3)) (type_convert (Ptype' p2 p3))
ftype_convert s (Ptype' p1 p2) = Fdef s (Sgl Em) (type_convert (Ptype' p1 p2))

type_convert :: Type -> NewType
type_convert (Ptype' (Ttype' t _) _) = Tpl (ttype_convert t)
type_convert (Ptype' (Xtype' (Xtype x [] _) _) _) = Sgl (btype_convert x) 
type_convert (Ptype' (Xtype' x _) _) = Dbl (xtype_convert x) 

ttype_convert :: Ttype -> [[BaseType]]
ttype_convert (Ttype [] _) = []
ttype_convert (Ttype ((Xtype x [] _):xs) p) = [[btype_convert x]] ++ (ttype_convert (Ttype xs p))
ttype_convert (Ttype (x:xs) p) =  [xtype_convert x] ++ (ttype_convert (Ttype xs p))


xtype_convert :: Xtype -> [BaseType]
xtype_convert (Xtype b s _) = [btype_convert b] ++ (map Sls s)

btype_convert :: Btype -> BaseType
btype_convert (Btype s _) = Base s

exists_in_sls :: [Def] -> String -> Bool
exists_in_sls [] _ = False
exists_in_sls ((Tdef s2 (Sgl (Sls _))):xs) s1 = if(s1 == s2)
                                                then True 
                                                else exists_in_sls xs s1
exists_in_sls ((Fdef s2 _ (Sgl (Sls _))):xs) s1 = if(s1 == s2)
                                                then True 
                                                else exists_in_sls xs s1
exists_in_sls (x:xs) s = exists_in_sls xs s

exists_in :: [Def] -> String -> Bool
exists_in [] _ = False
exists_in ((Tdef s2 _):xs) s1 = if(s1 == s2)
                                        then True 
                                        else exists_in xs s1
exists_in ((Fdef s2 _ _):xs) s1 = if(s1 == s2)
                                        then True 
                                        else exists_in xs s1

lookup_start :: [Def] -> String -> Bool
lookup_start d1 s = let (State _ d2) = builtin_state
                  in exists_in (d1 ++ d2) s

extract :: Def -> String
extract (Tdef s1 (Sgl (Base s2))) = s2
extract (Tdef s1 (Sgl (Sls s2))) = s2
extract _ = ""

d_filter :: Def -> [String]
d_filter (Tdef s1 (Sgl (Base s2))) = [s2]
d_filter (Tdef s1 (Sgl (Sls s2))) = [s2] 
d_filter (Tdef s1 (Dbl ((Base s2):xs))) = [s2]
d_filter (Tdef s1 (Dbl ((Em):xs))) = d_filter (Tdef s1 (Dbl xs))
d_filter (Tdef s1 (Dbl ((Sls s2):xs))) = d_filter (Tdef s1 (Dbl xs))
d_filter (Tdef s1 (Dbl [])) = [""]
d_filter _ = []

lookup5 :: [Def] -> String -> [Def]
lookup5 [] _ = []
lookup5 (d:ds) s = if(exists_in [d] s)
                        then [d] 
                        else (lookup5 ds s)

lookup4 :: [Def] -> String -> String
lookup4 [] _ = ""
lookup4 ((Tdef s2 t):xs) s1 = if(s1 == s2)
                                        then let s3 = d_filter (Tdef s2 t)
                                                in if(null s3)
                                                        then ""
                                                        else (head s3)
                                        else lookup4 xs s1
lookup4 ((Fdef s2 _ t):xs) s1 = lookup4 ([Tdef s2 t] ++ xs) s1

lookup3 :: [Def] -> String -> String
lookup3 [] _ = ""
lookup3 ((Tdef s2 t):xs) s1 = if(s1 == s2)
                                        then extract (Tdef s2 t) 
                                        else lookup3 xs s1
lookup3 ((Fdef s2 _ t):xs) s1 = lookup3 ([Tdef s2 t] ++ xs) s1

lookup2 :: [Def] -> String -> [Def]
lookup2 [] _ = []
lookup2 (d:ds) s = if(exists_in [d] s)
                        then [d] ++ (lookup2 ds s)
                        else (lookup2 ds s) ++ [d]

builtin_state :: State 
builtin_state = State  [Fdef "A" (Sgl Em) (Sgl (Sls "A")), 
                        Fdef "B" (Sgl Em) (Sgl (Sls "B")),
                        Fdef "or" (Tpl [[Base "Bool"], [Base "Bool"]]) (Sgl (Base "Bool")),
                        Fdef "not" (Sgl (Base "Bool")) (Sgl (Base "Bool")),
                        Fdef "inARow" (Tpl [[Base "Int"], [Base "Player"], [Base "Board"]]) (Sgl (Base "Bool")),
                        Fdef "input" (Sgl (Base "Board")) (Sgl (Base "Input")),
                        Fdef "isFull" (Sgl (Base "Board")) (Sgl (Base "Bool")),
                        Fdef "getBoardContent" (Tpl [[Base "Board"], [Base "Position"]]) (Dbl [Base "Player", Sls "Empty"]),
                        Fdef "place" (Tpl [[Base "Player"], [Base "Board"], [Base "Position"]]) (Sgl (Base "Board")),
                        Fdef "next" (Sgl (Base "Player")) (Sgl (Base "Player")),
                        Fdef "True" (Sgl Em) (Sgl (Base "Bool")),
                        Fdef "False" (Sgl Em) (Sgl (Base "Bool"))] 
                       [Tdef "Bool" (Sgl (Base "Bool")), 
                        Tdef "Int" (Sgl (Base "Int")), 
                        Tdef "Player" (Dbl [Sls "A", Sls "B"]), 
                        Tdef "Board" (Sgl (Base "Board")), 
                        Tdef "Position" (Tpl [[Base "Int"], [Base "Int"]])]

check_start :: [Stmt] -> (Bool, String) 
check_start [] = (True, "")
check_start x = let (state1, valid1, error1) = check_dup_name x (State [] [])
                        in if(valid1) 
                           then let (State fs ts, valid2, error2) = check_valid_def_types state1
                               in if(valid2)
                                        then let (valid3, error3) = check x (State fs ts)
                                                in (valid3, error3 ++ "\n")
                                        else (False, error2 ++ "\n")
                        else (False, error1 ++ "\n")

check :: [Stmt] -> State -> (Bool, String)
check [] _ = (True, "")
check (x:xs) st = let (state1, valid1, error1) = check_stmt x st
                      (valid2, error2) = check xs st
                        in (valid1 && valid2, error1 ++ error2)


peel :: String -> State -> String
peel s1 (State fs ts) = let s2 = lookup3 ts s1
                                 in if(null s2)
                                        then s1
                                        else if(s2 == s1)
                                                then s1
                                                else peel s2 (State fs ts)

check_binop :: Expr -> Binop -> Expr -> State -> (NewType, Bool, String)
check_binop e1 (Plus _) e2 st             = check_binop_help e1 e2 "PLUS" (Sgl (Base "Int")) st
check_binop e1 (Minus _) e2 st            = check_binop_help e1 e2 "Minus" (Sgl (Base "Int")) st
check_binop e1 (Times _) e2 st            = check_binop_help e1 e2 "Times" (Sgl (Base "Int")) st  
check_binop e1 (Div _) e2 st              = check_binop_help e1 e2 "Div" (Sgl (Base "Int")) st 
check_binop e1 (LessThan _) e2 st         = check_binop_help e1 e2 "LessThan" (Sgl (Base "Bool")) st 
check_binop e1 (GreaterThan _) e2 st      = check_binop_help e1 e2 "GreaterThan" (Sgl (Base "Bool")) st
check_binop e1 (LessThanEqual _) e2 st    = check_binop_help e1 e2 "LessThanEqual" (Sgl (Base "Bool")) st
check_binop e1 (GreaterThanEqual _) e2 st = check_binop_help e1 e2 "GreaterThanEqual" (Sgl (Base "Bool")) st
check_binop e1 (EqualTo _) e2 st          = let (type1, valid1, error1) = check_expr e1 st                                    
                                                (type2, valid2, error2) = check_expr e2 st
                                            in if((type_comp type1 type2 st) && (type_comp type2 type2 st))
                                                then (Sgl (Base "Bool"), valid1 && valid2, error1 ++ error2)
                                                else (Sgl Em, False, "\n" ++ (show type1) ++ " " ++ (show type2) ++ " are invalid args for function ==" ++ error1 ++ error2)                            

check_binop_help :: Expr -> Expr -> String -> NewType -> State -> (NewType, Bool, String)
check_binop_help e1 e2 s t st = let (type1, valid1, error1) = check_expr e1 st
                                    (type2, valid2, error2) = check_expr e2 st
                                    in if((type_comp type1 (Sgl (Base "Int")) st) && (type_comp type2 (Sgl (Base "Int")) st))
                                        then (t, valid1 && valid2, error1 ++ error2)
                                        else (Sgl Em, False,  "\n" ++ (show type1) ++ " " ++ (show type2) ++ " are invalid args for function " ++ s ++ error1 ++ error2)

tuple_concat :: NewType -> NewType -> NewType
tuple_concat (Sgl Em) _ = Sgl Em
tuple_concat _ (Sgl Em) = Sgl Em
tuple_concat (Sgl b1) (Sgl b2) = Tpl [[b1], [b2]]
tuple_concat (Sgl b1) (Dbl b2) = Tpl ([[b1]] ++ [b2])
tuple_concat (Sgl b1) (Tpl b2) = Tpl ([[b1]] ++ b2) 
tuple_concat (Dbl b1) (Dbl b2) = Tpl ([b1] ++ [b2])
tuple_concat (Dbl b1) (Tpl b2) = Tpl ([[Ifblk], b1, [Endif]] ++ b2)
tuple_concat (Tpl b1) (Tpl b2) = Tpl ([[Ifblk]] ++ b1 ++ [[Endif]] ++ b2)
tuple_concat t1 t2 = tuple_concat t2 t1

check_tuple :: [Expr] -> State -> (NewType, Bool, String)
check_tuple [x] st    = let (type1, valid1, error1) = check_expr x st
                            in (type1, valid1, error1)
check_tuple (x:xs) st = let (type1, valid1, error1) = check_expr x st
                            (type2, valid2, error2) = check_tuple xs st
                            in (tuple_concat type1 type2, valid1 && valid2, error1 ++ error2)

check_symbol :: String -> [Def] -> (NewType, Bool, String)
check_symbol s [x] =  check_symbol2 s x
check_symbol s (x:xs) = let (type1, valid1, error1) = check_symbol2 s x
                        in if(valid1) 
                            then (type1, valid1, error1)
                            else check_symbol s xs

check_symbol2 :: String -> Def -> (NewType, Bool, String)
check_symbol2 s1 (Fdef s2 (Sgl Em) t) = if(s1 == s2)
                                              then (t, True, "")
                                              else (Sgl Em, False, "\n" ++ s1 ++ " is undefined")
check_symbol2 s1 (Fdef s2 _ _) = if(s1 == s2)
                                              then (Sgl Em, False, "\n" ++ s1 ++ " is lacking arguments")
                                              else (Sgl Em, False, "\n" ++ s1 ++ " is undefined")



check_function_app :: String -> Expr -> State -> (NewType, Bool, String) 
check_function_app s1 e (State fs ts) = let d = lookup5 fs s1
                                                in if(null d)
                                                        then (Sgl Em, False, "\ncall to undefined function " ++ s1)
                                                        else let (type1, valid1, error1) = check_expr e (State fs ts)
                                                                 (Fdef s t2 t3) = head d
                                                                 in if(valid1)
                                                                        then if(type_comp type1 t2 (State fs ts))
                                                                                then (t3, True, "")
                                                                                else (Sgl Em, False, "\nCall to function " ++ s ++ " has incorrect input: " ++ (show type1) ++ " " ++ show(t2))
                                                                        else (type1, valid1, error1)
                                                                                        


check_expr :: Expr -> State -> (NewType, Bool, String)
check_expr (EInt _ _) st                          = (Sgl (Base "Int"), True, "")
check_expr (Paren e _) st                         = check_expr e st
check_expr (Tuple t _) st                         = check_tuple t st
check_expr (Infix e1 b e2 _) st                   = check_binop e1 b e1 st
check_expr (FunctionApp s e _) (State fs ts)      = check_function_app s e (State fs ts)
check_expr (ESymbol s _) (State fs ts)            = check_symbol s fs
check_expr (Empty) st                           = (Sgl Em, True, "")

bs_to_str :: BaseType -> String
bs_to_str (Base s) = s
bs_to_str (Sls s) = s
bs_to_str _ = ""

dbl_clean :: [BaseType] -> [BaseType] -> State -> [BaseType]
dbl_clean [] _ _ = []
dbl_clean (x:xs) bs st = if(exists_in_dbl bs (bs_to_str x) st)
                                then dbl_clean xs bs st
                                else [x] ++ (dbl_clean xs (bs ++ [x]) st)

condition_type_match :: NewType -> NewType -> State -> [NewType]
condition_type_match (Sgl Em) _ st = []
condition_type_match _ (Sgl Em) st = []
condition_type_match (Sgl (Base s1)) (Sgl (Base s2)) st = if((peel s1 st) == (peel s2 st))
                                                                then [Sgl (Base s1)]
                                                                else [Sgl Em]
condition_type_match (Sgl (Base s1)) (Dbl d) st = if(exists_in_dbl d s1 st)
                                                        then [Dbl d]
                                                        else [Dbl (d ++ [Base s1])]
condition_type_match (Sgl s) (Tpl t) st = [Tpl ([[Ifblk]] ++ [[s]] ++ [[Endif]] ++ t)]
condition_type_match (Sgl (Sls s)) (Sgl b) st = if(exists_in_dbl [b] s st)
                                                        then [Sgl b]
                                                        else [Dbl [Sls s,b]]
condition_type_match (Sgl (Sls s)) (Dbl d) st = if(exists_in_dbl d s st)
                                                        then [Dbl d]
                                                        else [Dbl ([Sls s] ++ d)]
condition_type_match (Dbl (d1:d1s)) (Dbl d2) st = if(exists_in_dbl d2 (bs_to_str d1) st)
                                                        then condition_type_match (Dbl d1s) (Dbl d2) st
                                                        else let [Dbl d3] = condition_type_match (Dbl d1s) (Dbl d2) st
                                                                in [Dbl ([d1] ++ d3)]
condition_type_match (Dbl []) (Dbl d2) st = [Dbl d2]
condition_type_match (Dbl d) (Tpl t) st  = [Tpl ([[Ifblk], d, [Endif]] ++ t)]
condition_type_match (Tpl t1) (Tpl t2) st = [Tpl ([[Ifblk]] ++ t1 ++ [[Endif]] ++ t2)]                                 
condition_type_match t1 t2 st = condition_type_match t2 t1 st


eparse :: [NewType] -> Bool
eparse [Sgl Em] = False
eparse _  = True

check_stmt :: Stmt -> State -> (NewType, Bool, String)
check_stmt (Conditional e stmt1 stmt2 _) st = let (type1, valid1, error1) = (check_expr e st)
                                                  (type2, valid2, error2) = check_stmt stmt1 st
                                                  (type3, valid3, error3) = check_stmt stmt2 st
                                                  in if(type_comp type1 (Sgl (Base "Bool")) st)
                                                        then let type4 = condition_type_match type2 type3 st
                                                                in if(null type4)
                                                                        then (Sgl Em, False, "\nConditional statements do not return the same type " ++ (show type2) ++ " " ++ (show type3) ++ error1 ++ error2 ++ error3)
                                                                        else (head type4, valid1 && valid2 && valid3, error1 ++ error2 ++ error3)
                                                                        
                                                        else (Sgl Em, False, "\nExpression " ++ (show e) ++ " is not of type Bool" ++ error1 ++ error2 ++ error3)
check_stmt (Let s e stmt _) (State fs ts)           = let (type1, valid1, error1) = check_expr e (State fs ts)
                                                        in if(eparse [type1])
                                                                then check_stmt stmt (State ([(Fdef s (Sgl Em) type1)] ++ fs) ts )
                                                                else (Sgl Em, valid1, error1)
check_stmt (While e1 e2 _) st          = let (type1, valid1, error1) = check_expr e1 st
                                             (type2, valid2, error2) = check_expr e2 st
                                        in if(type_comp type1 (Sgl (Base "Bool")) st)
                                            then (type2, valid1 && valid2, error1 ++ error2)
                                            else (Sgl Em, False, "\nExpression " ++ (show e1) ++ " is not of type Bool" ++ error1 ++ error2)
check_stmt (Valdef (Signature s t _) e _) (State fs ts)          = if(s == "initialBoard") 
                                                                        then check_initial_board e (head (lookup5 ts "@@@@")) (State fs ts)
                                                                        else check_func_call e (head (lookup5 fs s)) (State fs ts)
check_stmt (SExpr e _) st               = check_expr e st
check_stmt _ _                          = (Sgl Em, True, "")

tpl_arg_def :: String -> [Expr] -> [[BaseType]] -> State -> (State, Bool, String)
tpl_arg_def s1 [(ESymbol s2 _)] [t] (State fs ts) = if(exists_in (fs ++ ts) s2)
                                                        then (State fs ts, False, "\n variable name " ++ s2 ++ " is not valid in def of function " ++ s1)
                                                        else (State ([Fdef s2 (Sgl Em) (if((length t) == 1) then (Sgl (head t)) else (Dbl t))] ++ fs) ts, True, "")
tpl_arg_def s1 ((ESymbol s2 _):xs) (t:t2) (State fs ts) = let ((State fs1 ts1), valid1, error1) = tpl_arg_def s1 xs t2 (State fs ts)
                                                                in if(exists_in (fs1 ++ ts1) s2)
                                                                        then (State fs1 ts1, False, "\n variable name " ++ s2 ++ " is not valid in def of function " ++ s1)
                                                                        else (State ([Fdef s2 (Sgl Em) (if((length t) == 1) then (Sgl (head t)) else (Dbl t))] ++ fs1) ts1, True, "") 
                                                        


check_args :: Expr -> Def -> State -> (State, Bool, String)
check_args Empty (Fdef s (Sgl Em) t) st = (st, True, "")
check_args Empty (Fdef s _ _) st = (st, False, "\nNo arguments in left side of equation in def of function " ++ s)
check_args _ (Fdef s (Sgl Em) t) st= (st, False, "\nArguments given in left side of equation in def of function " ++ s ++ " that takes no arguments")
check_args (ESymbol s2 _) (Fdef s1 t1 t2) (State fs ts) = if(exists_in (fs ++ ts) s2)
                                                                then (State fs ts, False, "\n variable name " ++ s2 ++ " is not valid in def of function " ++ s1)
                                                                else (State ([Fdef s2 (Sgl Em) t1] ++ fs) ts, True, "")
check_args (Tuple e _) (Fdef s1 (Sgl (Base s2)) t1) (State fs ts) =  let (Tdef _ t2) = head (lookup5 (fs ++ ts) (peel s2 (State fs ts)))
                                                                                in case t2 of
                                                                                        (Tpl tp) -> if((length e) == (length tp))
                                                                                                        then tpl_arg_def s1 e tp (State fs ts)
                                                                                                        else (State fs ts, False, "\nnumber of arguments in def of function " ++ s1 ++ " do not match")
                                                                                        _ -> (State fs ts, False, "\nToo many argument given in left side of equation in def of function " ++ s1)
                                                                                        
check_args (Tuple e _) (Fdef s1 (Dbl d) t2) (State fs ts ) = (State fs ts, False, "\nnumber of arguments in def of function " ++ s1 ++ " do not match")
check_args (Tuple e _) (Fdef s1 (Tpl t1) t2) (State fs ts) =  if((length e) == (length t1))
                                                                then tpl_arg_def s1 e t1 (State fs ts)
                                                                else (State fs ts, False, "\nnumber of arguments in def of function " ++ s1 ++ " do not match")

check_initial_board_tpl :: Expr -> Bool
check_initial_board_tpl (Tuple [ESymbol s1 _, ESymbol s2 _] _) = (s1 == "x") && (s2 == "y")
check_initial_board_tpl (Tuple [ESymbol s _, EInt _ _] _) = s == "x"
check_initial_board_tpl (Tuple [EInt _ _, ESymbol s _] _) = s == "y"
check_initial_board_tpl (Tuple [EInt _ _, EInt _ _] _) = True
check_initial_board_tpl _ = False

check_initial_board :: [Equation] -> Def -> State -> (NewType, Bool, String)
check_initial_board [Equation s t stmt _] (Tdef _ t2) st = if(check_initial_board_tpl t)
                                                                                        then let (type1, valid1, error1) = check_stmt stmt st 
                                                                                                 in if(type_comp type1 t2 st)
                                                                                                         then (Sgl Em, valid1, error1)
                                                                                                         else (Sgl Em, False, "\nreturn type of initialBoard is not of type " ++ (show t2) ++ error1)
                                                                                        else (Sgl Em, False, "\narguments of initial board " ++ (show t) ++ " are not valid")
check_initial_board (x:xs) d st = let (type1, valid1, error1) = check_initial_board [x] d st
                                      (type2, valid2, error2) = check_initial_board xs d st
                                      in (type1, valid1 && valid2, error1 ++ error2)

check_func_call :: [Equation] -> Def -> State -> (NewType, Bool, String)
check_func_call [Equation s e stmt _] (Fdef _ t1 t2) (State fs ts) = let (state1, valid1, error1) = check_args e (Fdef s t1 t2) (State fs ts)
                                                                        in if(valid1)
                                                                                then let (type2, valid2, error2) = check_stmt stmt state1
                                                                                        in if(type_comp type2 t2 (State fs ts))
                                                                                                then (Sgl Em, valid2, error2)
                                                                                                else (Sgl Em, False, "\n return type of function " ++ s ++ " does not match" ++ error2) 
                                                                                else (Sgl Em, False, error1)
check_func_call ((Equation s e stmt _):xs) (Fdef _ t1 t2) (State fs ts) = let (state1, valid1, error1) = check_args e (Fdef s t1 t2) (State fs ts)
                                                        in if(valid1)
                                                                then let (type2, valid2, error2) = check_stmt stmt state1
                                                                         (type3, valid3, error3) = check_func_call xs (Fdef s t1 t2) (State fs ts)
                                                                         in if(type_comp type2 t2 (State fs ts))
                                                                                 then (Sgl Em, valid2 && valid3, error2 ++ error3)
                                                                                 else (Sgl Em, False, "\n return type of function " ++ s ++ " does not match" ++ error2 ++ error3) 
                                                                else let (type2, valid2, error2) = check_func_call xs (Fdef s t1 t2) (State fs ts)
                                                                         in (Sgl Em, False, error1 ++ error2)


check_valid_def_types :: State -> (State, Bool, String)
check_valid_def_types (State fs ts) = let (state1, valid1, error1) = check_type_types ts builtin_state
                                        in if(valid1) 
                                                then check_func_types fs state1
                                                else (state1, valid1, error1)

check_func_types :: [Def] -> State -> (State, Bool, String)
check_func_types [] st = (st, True, "")
check_func_types (x:xs) st = let (state1, valid1, error1) = check_func_types xs st
                                 in let (state2, valid2, error2) = check_func_types_helper x state1
                                        in (state2, valid1 && valid2, error1 ++ error2)

check_func_types_helper :: Def -> State -> (State, Bool, String)
check_func_types_helper (Fdef s ti to) (State fs ts) = let (state1, type1, valid1, error1) = full_type_to_str s ti (State fs ts)
                                                           in let ((State fs2 ts2), type2, valid2, error2) = full_type_to_str s to state1 
                                                                  in if(valid1 && valid2)
                                                                        then (State (fs2 ++ [Fdef s type1 type2]) ts2, True, "")
                                                                        else (State fs2 ts2, False, error1 ++ error2)
full_type_to_str :: String -> NewType -> State -> (State, NewType, Bool, String)
full_type_to_str _ (Sgl Em) st = (st, (Sgl Em), True, "")
full_type_to_str fstr (Sgl (Base s)) (State fs ts) = if(exists_in_sls fs s) 
                                                        then (State fs ts, (Sgl Em), False, "\n1 -> Invalid type " ++ s ++ " in function " ++ fstr)
                                                        else if(exists_in ts s)
                                                                then (State fs ts, Sgl (Base s), True, "")
                                                                else (State fs ts, (Sgl Em), False, "\n Type " ++ s ++ " in function " ++ fstr ++ " is undefined")
full_type_to_str fstr (Dbl x) st = let (state1, type1, valid1, error1) = full_type_to_str_x_pre fstr x st 
                                        in (state1, Dbl type1, valid1, error1)
full_type_to_str fstr (Tpl (x:xs)) st = let (state1, type1, valid1, error1) = full_type_to_str_x_pre fstr x st
                                              in let (state2, (Tpl x2), valid2, error2) = full_type_to_str fstr (Tpl xs) st
                                                     in (state2, Tpl ([type1] ++ x2), valid1 && valid2, error1 ++ error2)
full_type_to_str fstr (Tpl []) st = (st, Tpl [], True, "")

full_type_to_str_x_pre :: String -> [BaseType] -> State -> (State, [BaseType], Bool, String)
full_type_to_str_x_pre fstr [] st = (st, [], True, "")
full_type_to_str_x_pre fstr ((Base s):xs) (State fs ts) = if(exists_in ts s)
                                                                        then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                 in (state1, [Base s] ++ type1, valid1, error1)
                                                                        else if(exists_in_sls fs s)
                                                                                then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                         in (state1, [Sls s] ++ type1, valid1, error1)
                                                                                else if(exists_in fs s)
                                                                                        then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                                 in (state1, [], False, "\n in definition of function " ++ fstr ++ " name " ++ s ++ " already in use" ++ error1)
                                                                                        else let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State (fs ++ [Fdef s (Sgl Em) (Sgl (Sls s))]) ts)
                                                                                                 in (state1, [Sls s] ++ type1, valid1, error1)

full_type_to_str_x :: String -> [BaseType] -> State -> (State, [BaseType], Bool, String)
full_type_to_str_x fstr ((Sls s):xs) (State fs ts) = if(exists_in ts s)
                                                                then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                         in (state1, [], False, "\n value having type only allowed in first arg of |: " ++ s ++ " in function " ++ fstr ++ error1)
                                                                else if(exists_in_sls fs s)
                                                                        then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                 in (state1, [Sls s] ++ type1, valid1, error1)
                                                                        else if(exists_in fs s)
                                                                                then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                         in (state1, [], False, "\n in definition of function " ++ fstr ++ " name " ++ s ++ " already in use" ++ error1)
                                                                                else let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State (fs ++ [Fdef s (Sgl Em) (Sgl (Sls s))]) ts)
                                                                                         in (state1, [Sls s] ++ type1, valid1, error1)
full_type_to_str_x fstr [] st = (st, [], True, "")


check_type_types :: [Def] -> State -> (State, Bool, String)
check_type_types [] st = (st, True, "")
check_type_types (x:xs) st = let (defs, state, valid, error) = check_type_types_helper [x] xs st
                                in (state, valid, error)

check_type_types_helper :: [Def] -> [Def] -> State -> ([Def], State, Bool, String)
check_type_types_helper [] d st = (d, st, True, "")
check_type_types_helper [(Tdef s1 t)] d st = let (type1, defs1, state1, valid1, error1) = ast_convert [s1] t d st
                                                        in if(null defs1) 
                                                                then (defs1, state1, valid1, error1)
                                                                else check_type_types_helper [head defs1] (tail defs1) state1

ast_convert :: [String] -> NewType -> [Def] -> State -> (NewType, [Def], State, Bool, String)
ast_convert s (Sgl sg) d st = let(type1, defs1, (State fs ts), valid1, error1) = ast_convert_b s sg d True st 
                                in if(valid1)
                                        then (Sgl (Base (head s)), defs1, State fs (([Tdef (head s) type1]) ++ ts), valid1, error1)
                                        else (Sgl Em, defs1, State fs ts, valid1, error1)
ast_convert s (Dbl x) d st = let (type1, defs1, (State fs ts), valid1, error1) = ast_convert_x s x d st
                                                in if(valid1)
                                                        then (Sgl (Base (head s)), defs1, State fs (([Tdef (head s) type1]) ++ ts), valid1, error1)
                                                        else (Sgl Em, defs1, State fs ts, valid1, error1)
ast_convert s (Tpl x) d st = let (Tpl x1, defs1, (State fs1 ts1), valid1, error1) = ast_convert_t s x d st
                                   in let ((Dbl b2), defs2, (State fs2 ts2), valid2, error2) = ast_convert_x s b2 defs1 (State fs1 ts1) 
                                             in if(valid1 && valid2)
                                                     then (Sgl (Base (head s)), defs1, State fs2 (([Tdef (head s) (Tpl x1)]) ++ ts2), valid2 && valid1, error1 ++ error2 )
                                                     else (Sgl Em, defs2, (State fs2 ts2), False, error2 ++ error1)
                                        

ast_convert_t :: [String] -> [[BaseType]] -> [Def] -> State -> (NewType, [Def], State, Bool, String)
ast_convert_t s [] d st = (Tpl [], d, st, True, "")
ast_convert_t s (x:xs) d st = let (Dbl x1, defs1, state1, valid1, error1) = ast_convert_x s x d st 
                                        in let (Tpl x2, defs2, state2, valid2, error2) = ast_convert_t s xs defs1 state1
                                                in (Tpl (x2 ++ [x1]), defs2, state2, valid1 && valid2, error1 ++ error2)

ast_convert_x :: [String] -> [BaseType] -> [Def] -> State -> (NewType, [Def], State, Bool, String)
ast_convert_x s [b] d st = ast_convert_b s b d True st
ast_convert_x s (b:bs) d st = let (Sgl base1, defs1, state1, valid1, error1) = ast_convert_b s b d False st 
                                        in let (Dbl base2, defs2, state2, valid2, error2) = ast_convert_x2 s bs defs1 state1
                                                in (Dbl ([base1] ++ base2), defs2, state2, valid1 && valid2, error1 ++ error2)

ast_convert_x2 :: [String] -> [BaseType] -> [Def] -> State -> (NewType, [Def], State, Bool, String)
ast_convert_x2 s [b] d st = let (Sgl bas, def1, state1, valid1, error1) = ast_convert_b s b d False st
                                in (Dbl [bas], def1, state1, valid1, error1)
ast_convert_x2 s (b:bs) d st = let (Sgl base1, defs1, state1, valid1, error1) = ast_convert_b s b d False st 
                                        in let (Dbl base2, defs2, state2, valid2, error2) = ast_convert_x2 s bs defs1 state1
                                                in (Dbl ([base1] ++ base2), defs2, state2, valid1 && valid2, error1 ++ error2)

ast_convert_b :: [String] -> BaseType -> [Def] -> Bool -> State -> (NewType, [Def], State, Bool, String)
ast_convert_b s1 (Sls s2) d _ (State fs ts) = if(exists_in fs s2)
                                                                then (Sgl (Sls s2), d, State fs ts, True, "")
                                                                else if(exists_in ts s2)
                                                                        then (Sgl Em, d, State fs ts, False, "\n Value containing types can only be used on the left side of |")
                                                                        else (Sgl (Sls s2), d, State ([Fdef s2 (Sgl Em) (Sgl (Sls s2))] ++ fs) ts, True, "")
ast_convert_b s1 (Base s2) d singleFlag (State fs ts) = if(exists_in ts s2)
                                                                then (Sgl (Sls s2), d, State fs ts, True, "")
                                                                else if(exists_in fs s2)
                                                                        then if(singleFlag)
                                                                                then (Sgl Em, d, State fs ts, False, "\nyou really shouldn't")
                                                                                else (Sgl (Sls s2), d, State fs ts, True, "")
                                                                        else if (exists_in d s2)
                                                                                then let ((Tdef s3 t2):dfs) = lookup2 d s2
                                                                                        in ast_convert ([s3] ++ s1) t2 dfs (State fs ts)
                                                                                else if(and (map ((==) s2) s1))
                                                                                        then (Sgl Em, d, State fs ts, False, "\nType definition of type " ++ s2 ++ " cannot refrence the following types " ++ (intercalate " " s1))
                                                                                        else if(singleFlag)
                                                                                                then (Sgl Em, d, State fs ts, False, "\nyou really shouldn't do that")
                                                                                                else (Sgl (Base s2), d, State ([Fdef s2 (Sgl Em) (Sgl (Sls s2))] ++ fs) ts, True, "")  

check_dup_name :: [Stmt] -> State -> (State, Bool, String)
check_dup_name [] st = (st, True, "")
check_dup_name (x:xs) st = let (state1, valid1, error1) = check_dup_name_stmt x st
                            in let (state2, valid2, error2) = check_dup_name xs state1
                                    in (state2, valid1 && valid2, error2 ++ error1)

check_equation_name :: String -> [Equation] -> Bool
check_equation_name s1 [(Equation s2 _ _ _)] = s1 == s2
check_equation_name s1 ((Equation s2 _ _ _):xs) = (s1 == s2) && (check_equation_name s1 xs)

check_dup_name_stmt :: Stmt -> State -> (State, Bool, String) 
check_dup_name_stmt (Typedef s t _) (State fs ts)  =  if(lookup_start (fs ++ ts) s)
                                                        then (State fs ts, False, "\nType name " ++ s ++ " already used")
                                                        else (State fs ([Tdef s (type_convert t)] ++ ts), True, "")                                                
check_dup_name_stmt (Valdef (Signature s t _) e _) (State fs ts) = if(lookup_start (fs ++ ts) s)
                                                                then (State fs ts, False, "\nFunction name " ++ s ++ " already used")
                                                                else if(check_equation_name s e)
                                                                        then (State ([ftype_convert s t] ++ fs) ts, True, "")
                                                                        else (State fs ts, False, "\nFunction " ++ s ++ " has inconsistent naming" )                                                                
check_dup_name_stmt (TypedefFunc s e t _) (State fs ts)  = if(not (s == "Board"))
                                                then ((State fs ts), False, "\nof type not allowed for non board definitions")
                                                else (State fs ([Tdef "@@@@" (type_convert t)] ++ ts), True, "")
check_dup_name_stmt _ st = (st, True, "")