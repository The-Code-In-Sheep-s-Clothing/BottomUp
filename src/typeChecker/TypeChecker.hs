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
               | Tplblk
               | Endtpl
               | Em
               deriving (Show,Eq)

check_start :: [Stmt] -> (Bool, String) 
check_start [] = (True, "")
check_start x = let (state1, valid1, error1) = check_dup_name x (State [] [])
                        in if(valid1) 
                           then let (State fs ts, valid2, error2) = check_valid_def_types state1
                               in if(valid2)
                                        then let (valid3, error3) = check x (State fs ts)
                                                in (valid3, error3)
                                        else (False, error2 ++ "\n")
                        else (False, error1 ++ "\n")

check_stmt :: Stmt -> State -> (NewType, Bool, String)
check_stmt (Conditional e stmt1 stmt2 _) st = let (type1, valid1, error1) = (check_expr e st)
                                                  (type2, valid2, error2) = check_stmt stmt1 st
                                                  (type3, valid3, error3) = check_stmt stmt2 st
                                                  in if(type_comp type1 (Sgl (Base "Bool")) st)
                                                        then let type4 = condition_type_match type2 type3 st
                                                                in if(eparse type4)
                                                                        then if(null type4)
                                                                                then (Sgl Em, valid1 && valid2 && valid3, error1 ++ error2 ++ error3)
                                                                                else (head type4, valid1 && valid2 && valid3, error1 ++ error2 ++ error3) 
                                                                        else (Sgl Em, False, "\nConditional statements do not return the same type. Then type: " ++ (t_to_s type2) ++ " Else Type: " ++ (t_to_s type3) ++ error1 ++ error2 ++ error3)
                                                                        
                                                                        
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
check_stmt (Valdef (Signature s t _) e _) (State fs ts)          = if(check_type_init_board e) 
                                                                        then let (type1, bcheck1, valid1, error1) = (check_initial_board e (head (lookup5 ts "@@@@")) (form_board (head (lookup5 ts "&&&&"))) (not((length e) == 1)) (State fs ts))
                                                                                 in (type1, valid1, error1)
                                                                        else check_func_call e (head (lookup5 fs s)) (State fs ts)
check_stmt (SExpr e _) st               = check_expr e st
check_stmt _ _                          = (Sgl Em, True, "")

check_expr :: Expr -> State -> (NewType, Bool, String)
check_expr (EInt _ _) st                          = (Sgl (Base "Int"), True, "")
check_expr (Paren e _) st                         = check_expr e st
check_expr (ETuple t _) st                        = check_tuple t st
check_expr (Infix e1 b e2 _) st                   = check_binop e1 b e2 st
check_expr (FunctionApp s e _) (State fs ts)      = check_function_app s e (State fs ts)
check_expr (ESymbol s _) (State fs ts)            = check_symbol s fs
check_expr (Empty) st                           = (Sgl Em, True, "")

check_type_types :: [Def] -> State -> (State, Bool, String)
check_type_types [] st = (st, True, "")
check_type_types (x:xs) st = let (defs, state, valid, error) = check_type_types_helper [x] xs st
                                in (state, valid, error)

check_type_types_helper :: [Def] -> [Def] -> State -> ([Def], State, Bool, String)
check_type_types_helper [] d st = (d, st, True, "")
check_type_types_helper [(Tdef "&&&&" t)] _ (State fs ts) = ([], State fs ([Tdef "&&&&" t] ++ ts), True, "")
check_type_types_helper [(Tdef s1 t)] d st = let (type1, defs1, state1, valid1, error1) = ast_convert [s1] t d st
                                                        in if(valid1)
                                                                then if(null defs1) 
                                                                        then (defs1, state1, valid1, error1)
                                                                        else let (defs2, state2, valid2, error2) = check_type_types_helper [head defs1] (tail defs1) state1
                                                                                in (defs2, state2, valid1 && valid2, error1 ++ error2)
                                                                else (defs1, state1, valid1, error1)

ast_convert :: [String] -> NewType -> [Def] -> State -> (NewType, [Def], State, Bool, String)
ast_convert s (Sgl sg) d st = let(type1, defs1, (State fs ts), valid1, error1) = ast_convert_b s sg d False st 
                                in if(valid1)
                                        then (Sgl (Base (head s)), defs1, State fs (([Tdef (head s) type1]) ++ ts), valid1, error1)
                                        else (Sgl Em, defs1, State fs ts, valid1, error1)
ast_convert s (Dbl x) d st = let (type1, defs1, (State fs ts), valid1, error1) = ast_convert_x s x d st
                                                in if(valid1)
                                                        then (Sgl (Base (head s)), defs1, State fs (([Tdef (head s) type1]) ++ ts), valid1, error1)
                                                        else (Sgl Em, defs1, State fs ts, valid1, error1)
ast_convert s (Tpl x) d st = let (Tpl x1, defs1, (State fs1 ts1), valid1, error1) = ast_convert_t s x d st
                                        in if(valid1)
                                                then (Sgl (Base (head s)), defs1, State fs1 (([Tdef (head s) (Tpl x1)]) ++ ts1), valid1, error1)
                                                else (Sgl Em, defs1, (State fs1 ts1), False,  error1)
                                        

ast_convert_t :: [String] -> [[BaseType]] -> [Def] -> State -> (NewType, [Def], State, Bool, String)
ast_convert_t s [] d st = (Tpl [], d, st, True, "")
ast_convert_t s (x:xs) d st = let (Dbl x1, defs1, state1, valid1, error1) = ast_convert_x s x d st 
                                        in let (Tpl x2, defs2, state2, valid2, error2) = ast_convert_t s xs defs1 state1
                                                in (Tpl (x2 ++ [x1]), defs2, state2, valid1 && valid2, error1 ++ error2)

ast_convert_x :: [String] -> [BaseType] -> [Def] -> State -> (NewType, [Def], State, Bool, String)
ast_convert_x s [b] d st = ast_convert_b s b d False st
ast_convert_x s (b:bs) d st = let (Sgl base1, defs1, state1, valid1, error1) = ast_convert_b s b d False st 
                                        in let (Dbl base2, defs2, state2, valid2, error2) = ast_convert_x2 s bs defs1 state1
                                                in (Dbl ([base1] ++ base2), defs2, state2, valid1 && valid2, error1 ++ error2)

ast_convert_x2 :: [String] -> [BaseType] -> [Def] -> State -> (NewType, [Def], State, Bool, String)
ast_convert_x2 s [b] d st = let (Sgl bas, def1, state1, valid1, error1) = ast_convert_b s b d True st
                                in (Dbl [bas], def1, state1, valid1, error1)
ast_convert_x2 s (b:bs) d st = let (Sgl base1, defs1, state1, valid1, error1) = ast_convert_b s b d True st 
                                        in let (Dbl base2, defs2, state2, valid2, error2) = ast_convert_x2 s bs defs1 state1
                                                in (Dbl ([base1] ++ base2), defs2, state2, valid1 && valid2, error1 ++ error2)

ast_convert_b :: [String] -> BaseType -> [Def] -> Bool -> State -> (NewType, [Def], State, Bool, String)
ast_convert_b s1 (Sls s2) d firstFlag (State fs ts) = if(exists_in ts s2) 
                                                                then if(firstFlag && (check_base ts s2 ts))
                                                                        then (Sgl Em, d, State fs ts, False, "\nType " ++ s2 ++ " in definition of type " ++ (head s1) ++ ". Types can only be expanded by Values")
                                                                        else (Sgl (Base s2), d, State fs ts, True, "")
                                                                else if(exists_in fs s2)
                                                                        then (Sgl Em, d, State fs ts, False, "\nValue " ++ s2 ++ " cannot be refreneced in two different type definitions")
                                                                        else if(exists_in d s2)
                                                                                then let ((Tdef s3 t2):dfs) = lookup2 d s2
                                                                                                in ast_convert ([s3] ++ s1) t2 dfs (State fs ts)
                                                                                else if(and (map ((==) s2) s1))
                                                                                        then (Sgl Em, d, State fs ts, False, "\nType definition of type " ++ s2 ++ " cannot refrence the following types " ++ (intercalate " " s1))
                                                                                        else (Sgl (Sls s2), d, State ([Fdef s2 (Sgl Em) (Sgl (Sls s2))] ++ fs) ts, True, "")                                                                           
ast_convert_b s1 (Base s2) d _ (State fs ts) = if(exists_in ts s2)
                                                        then (Sgl (Base s2), d, State fs ts, True, "")
                                                        else if(exists_in fs s2)
                                                                then (Sgl Em, d, State fs ts, False, "\nType declarations using values must use {}")
                                                                else if (exists_in d s2)
                                                                        then let ((Tdef s3 t2):dfs) = lookup2 d s2
                                                                                in ast_convert ([s3] ++ s1) t2 dfs (State fs ts)
                                                                        else (Sgl Em, d, State fs ts, False, "\nReference to undefined type " ++ s2 ++ " in type definition")

check_base :: [Def] -> String -> [Def] -> Bool
check_base [d] s ds = check_base2 ds d
check_base ((Tdef s1 t):xs) s2 ds = if(s1 == s2)
                                        then check_base2 ds (Tdef s1 t) 
                                        else check_base xs s2 ds

check_base2 :: [Def] -> Def -> Bool
check_base2 _ (Tdef _ (Sgl (Base "Int"))) = True
check_base2 _ (Tdef _ (Sgl (Base "Board"))) = True
check_base2 _ (Tdef _ (Sgl (Base "Bool"))) = True
check_base2 ds (Tdef _ (Sgl (Base s2))) = check_base2 ds (head (lookup5 ds s2)) 
check_base2 _ (Tdef _ (Sgl (Sls _))) = False
check_base2 ds (Tdef s1 (Dbl (d:_))) = check_base2 ds (Tdef s1 (Sgl d))
check_base2 ds (Tdef s1 (Tpl ([Tplblk]:ts))) = let (ts1, ts2) = tpl_extract ts
                                                   in (check_base2 ds (Tdef s1 (Tpl ts1))) || (check_base2 ds (Tdef s1 (Tpl ts2)))
check_base2 ds (Tdef s1 (Tpl (t:ts))) = (check_base2 ds (Tdef s1 (Dbl t))) || (check_base2 ds (Tdef s1 (Tpl ts)))
check_base2 ds (Tdef s1 (Tpl [])) = False

check_dup_name :: [Stmt] -> State -> (State, Bool, String)
check_dup_name [] st = (st, True, "")
check_dup_name (x:xs) st = let (state1, valid1, error1) = check_dup_name_stmt x st
                            in let (state2, valid2, error2) = check_dup_name xs state1
                                    in (state2, valid1 && valid2, error2 ++ error1)

check_equation_name :: String -> [Equation] -> Bool
check_equation_name s1 [(Equation s2 _ _ _)] = s1 == s2
check_equation_name s1 ((Equation s2 _ _ _):xs) = (s1 == s2) && (check_equation_name s1 xs)
check_equation_name s1 [(ArrayEquation s2 _ _ _)] = s1 == s2
check_equation_name s1 ((ArrayEquation s2 _ _ _):xs) = (s1 == s2) && (check_equation_name s1 xs)

check_dup_name_stmt :: Stmt -> State -> (State, Bool, String) 
check_dup_name_stmt (Typedef s t _) (State fs ts)  =  if(lookup_start (fs ++ ts) s)
                                                        then (State fs ts, False, "\nType name " ++ s ++ " already used")
                                                        else (State fs ([Tdef s (type_convert t)] ++ ts), True, "")                                                
check_dup_name_stmt (Valdef (Signature s t _) e _) (State fs ts) = if(lookup_start (fs ++ ts) s)
                                                                then (State fs ts, False, "\nFunction name " ++ s ++ " already used")
                                                                else if(check_equation_name s e)
                                                                        then let d = ftype_convert s t
                                                                                in (State ([d] ++ fs) ts, True, "")
                                                                        else (State fs ts, False, "\nFunction " ++ s ++ " has inconsistent naming" )                                                                
check_dup_name_stmt (TypedefFunc s (FunctionApp _ e _) t _) (State fs ts)  = if(not (s == "Board"))
                                                                                then ((State fs ts), False, "\nof type not allowed for non board definitions")
                                                                                else case e of 
                                                                                        (ETuple (TupleList [TupleValue (EInt v1 _) _, TupleValue (EInt v2 _) _] _) _) -> (State fs ([Tdef "@@@@" (type_convert t)] ++ [Tdef "&&&&" (Dbl [Base (show v1), Base (show v2)])] ++ ts), True, "")
                                                                                        _ -> (State fs ts, False, "\nboard size must be defined by (int, int) " ++ (show e))
check_dup_name_stmt _ st = (st, True, "")


t_to_s :: NewType -> String
t_to_s t = "(" ++ (t_to_s2 t) ++ ")" 

t_to_s2 :: NewType -> String
t_to_s2 (Sgl Em) = "Type Error"
t_to_s2 (Sgl (Base s)) = s
t_to_s2 (Sgl (Sls s)) = s
t_to_s2 (Dbl [d]) = t_to_s2 (Sgl d)
t_to_s2 (Dbl (d:ds)) = (t_to_s2 (Sgl d)) ++ " & " ++ (t_to_s2 (Dbl ds))
t_to_s2 (Tpl ([Tplblk]:ts)) = let (ts1, ts2) = tpl_extract ts
                                 in "(" ++ ((t_to_s2 (Tpl ts1))) ++ ")," ++ (t_to_s2 (Tpl ts2))
t_to_s2 (Tpl [t]) = ((t_to_s2 (Dbl t)))                                 
t_to_s2 (Tpl (t:ts)) = ((t_to_s2 (Dbl t))) ++ "," ++ (t_to_s2 (Tpl ts))
t_to_s2 (Tpl []) = ""

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
if_extract ([Endif]:xs) = ([], xs)
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


tpl_extract :: [[BaseType]] -> ([[BaseType]], [[BaseType]])
tpl_extract ([Endtpl]:xs) = ([], xs)
tpl_extract (x:xs) = let (b1, b2) = tpl_extract xs
                        in ([x] ++ b1, b2)

tpl_comp :: [[BaseType]] -> [[BaseType]] -> State -> Bool
tpl_comp ([Ifblk]:bs1) b2 st = let (bs11, bs12) = if_extract bs1
                                   in (tpl_comp bs11 b2 st) && (tpl_comp bs12 b2 st)
tpl_comp b1 ([Ifblk]:bs2) st = let (bs21, bs22) = if_extract bs2
                                   in (tpl_comp b1 bs21 st) && (tpl_comp b1 bs22 st)
tpl_comp ([Tplblk]:bs1) ([Tplblk]:bs2) st = let (bs11, bs12) = tpl_extract bs1
                                                (bs21, bs22) = tpl_extract bs2
                                                in (tpl_comp bs11 bs21 st) && (tpl_comp bs12 bs22 st)
tpl_comp ([Tplblk]:bs1) (bs21:bs22) st = let (bs11, bs12) = tpl_extract bs1
                                             in (tpl_comp bs11 [bs21] st) && (tpl_comp bs12 bs22 st)
tpl_comp (bs11:bs12) ([Tplblk]:bs2) st = let (bs21, bs22) = tpl_extract bs2 
                                             in (tpl_comp [bs11] bs21 st) && (tpl_comp bs12 bs22 st) 
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

ptype_convert :: Ptype -> [[BaseType]]
ptype_convert (Ttype' t _) = ([[Tplblk]] ++ (ttype_convert t) ++ [[Endtpl]])
ptype_convert (Xtype' (Xtype x [] _) _) = [[btype_convert x]] 
ptype_convert (Xtype' x _) = [xtype_convert x]  

ttype_convert :: Ttype -> [[BaseType]]
ttype_convert (Ttype t _) = (concat (map ptype_convert t))


xtype_convert :: Xtype -> [BaseType]
xtype_convert (Xtype b s _) = [btype_convert b] ++ (map Sls s)
xtype_convert (Etype s _) = map Sls s

btype_convert :: Btype -> BaseType
btype_convert (Btype s _) = Base s

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



check :: [Stmt] -> State -> (Bool, String)
check [] _ = (True, "")
check (x:xs) st = let (state1, valid1, error1) = (check_stmt x st)
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
check_binop e1 (Bang _) e2 (State fs ts)  = let (type1, valid1, error1) = check_expr e1 (State fs ts)                                    
                                                (type2, valid2, error2) = check_expr e2 (State fs ts)
                                                in if((type_comp type1 (Sgl (Base "Board")) (State fs ts)) && (type_comp type2 (Sgl (Base "Position")) (State fs ts)))
                                                        then let (Tdef _ t) = (head (lookup5 ts "@@@@"))
                                                                 in (t, valid1 && valid2, error1 ++ error2)
                                                        else (Sgl Em, False, "\n" ++ (t_to_s type1) ++ " " ++ (t_to_s type2) ++ " are invalid args for function !" ++ error1 ++ error2)  
check_binop e1 (EqualTo _) e2 st          = let (type1, valid1, error1) = check_expr e1 st                                    
                                                (type2, valid2, error2) = check_expr e2 st
                                            in if((type_comp type1 type2 st) || (type_comp type2 type2 st))
                                                then (Sgl (Base "Bool"), valid1 && valid2, error1 ++ error2)
                                                else (Sgl Em, False, "\n" ++ (t_to_s type1) ++ " " ++ (t_to_s type2) ++ " are invalid args for function ==" ++ error1 ++ error2)                            

check_binop_help :: Expr -> Expr -> String -> NewType -> State -> (NewType, Bool, String)
check_binop_help e1 e2 s t st = let (type1, valid1, error1) = check_expr e1 st
                                    (type2, valid2, error2) = check_expr e2 st
                                    in if((type_comp type1 (Sgl (Base "Int")) st) && (type_comp type2 (Sgl (Base "Int")) st))
                                        then (t, valid1 && valid2, error1 ++ error2)
                                        else (Sgl Em, False,  "\n" ++ (t_to_s type1) ++ " " ++ (t_to_s type2) ++ " are invalid args for function " ++ s ++ error1 ++ error2)

tuple_concat :: NewType -> NewType -> NewType
tuple_concat (Sgl Em) _ = Sgl Em
tuple_concat _ (Sgl Em) = Sgl Em
tuple_concat (Sgl b1) (Sgl b2) = Tpl [[b1], [b2]]
tuple_concat (Sgl b1) (Dbl b2) = Tpl ([[b1]] ++ [b2])
tuple_concat (Sgl b1) (Tpl b2) = Tpl ([[b1]] ++ b2) 
tuple_concat (Dbl b1) (Sgl b2) = Tpl ([b1, [b2]])
tuple_concat (Dbl b1) (Dbl b2) = Tpl ([b1] ++ [b2])
tuple_concat (Dbl b1) (Tpl b2) = Tpl ([b1] ++ b2)
tuple_concat (Tpl b1) (Sgl b2) = Tpl (b1 ++ [[b2]])
tuple_concat (Tpl b1) (Dbl b2) = Tpl (b1 ++ [b2])
tuple_concat (Tpl b1) (Tpl b2) = Tpl ([[Tplblk]] ++ b1 ++ [[Endtpl]] ++ b2)


check_tuple :: Tuple -> State -> (NewType, Bool, String)
check_tuple (TupleValue x _) st          = check_expr x st
check_tuple (TupleList [x] _) st         = check_tuple x st
check_tuple (TupleList (x:xs) pos) st    = let (type1, valid1, error1) = check_tuple x st
                                               (type2, valid2, error2) = check_tuple (TupleList xs pos) st
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
                                              then (Sgl Em, False, "\nNo arguments given for call to function " ++ s1)
                                              else (Sgl Em, False, "\n" ++ s1 ++ " is undefined")



check_function_app :: String -> Expr -> State -> (NewType, Bool, String) 
check_function_app s1 e (State fs ts) = let d = lookup5 fs s1
                                                in if(null d)
                                                        then (Sgl Em, False, "\ncall to undefined function " ++ s1)
                                                        else let (type1, valid1, error1) = check_expr e (State fs ts)
                                                                 (Fdef s t2 t3) = (head d)
                                                                 in if(valid1)
                                                                        then if(type_comp type1 t2 (State fs ts))
                                                                                then (t3, True, "")
                                                                                else (Sgl Em, False, "\nCall to function " ++ s ++ " has incorrect arguments. Given: " ++ (t_to_s type1) ++ " Expected: " ++ (t_to_s t2))
                                                                        else (type1, valid1, error1)
                                                                                        




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

check_type_init_board :: [Equation] -> Bool
check_type_init_board ((ArrayEquation _ _ _ _):xs) = True
check_type_init_board _ = False


tpl_len :: [[BaseType]] -> Int
tpl_len ([Tplblk]:xs) = 1 + tpl_len (snd (tpl_extract xs))
tpl_len (x:xs) = 1 + (tpl_len xs)

tpl_len_comp2 :: [Tuple] -> [[BaseType]] -> State -> (State, Bool, String)
tpl_len_comp2 [] [] st = (st, True, "")
tpl_len_comp2 [] x st = (st, False, "")
tpl_len_comp2 x [] st = (st, False, "")
tpl_len_comp2 (x:xs) ([Tplblk]:t) st = let (t11, t12) = tpl_extract t
                                           (state1, valid1, error1) = tpl_len_comp x t11 st
                                           in if(valid1) 
                                                   then tpl_len_comp2 xs t12 state1
                                                   else (st, False, error1)
tpl_len_comp2 (x:xs) (t:ts) st = let (state1, valid1, error1) = tpl_len_comp x [t] st 
                                     in if(valid1)
                                                then tpl_len_comp2 xs ts state1
                                                else (st, False, error1) 

tpl_len_comp :: Tuple -> [[BaseType]] -> State -> (State, Bool, String)
tpl_len_comp (TupleValue (ESymbol s _) _) [[t]] (State fs ts) = if(exists_in (fs ++ ts) s)
                                                                        then (State fs ts, False, "\nSymbol name " ++ s ++ " has already used in this scope")
                                                                        else (State ([Fdef s (Sgl Em) (Sgl t)] ++ fs) ts, True, "")
tpl_len_comp (TupleValue (ESymbol s _) _) [t] (State fs ts) = if(exists_in (fs ++ ts) s)
                                                                        then (State fs ts, False, "\nSymbol name " ++ s ++ " has already used in this scope")
                                                                        else (State ([Fdef s (Sgl Em) (Dbl t)] ++ fs) ts, True, "")
tpl_len_comp (TupleValue (ESymbol s _) _) t (State fs ts) = if(exists_in (fs ++ ts) s)
                                                                        then (State fs ts, False, "\nSymbol name " ++ s ++ " has already used in this scope")
                                                                        else if((tpl_len t) == 1) 
                                                                                then (State ([Fdef s (Sgl Em) (Tpl t)] ++ fs) ts, True, "")
                                                                                else (State fs ts, False, "")
tpl_len_comp (TupleList (x:xs) _) ([Tplblk]:t) st = let (t11, t12) = tpl_extract t
                                                        in let (state1, valid1, error1) = tpl_len_comp x t11 st
                                                               in if(valid1) 
                                                                       then tpl_len_comp2 xs t12 state1
                                                                       else (st, False, error1) 
tpl_len_comp (TupleList x _) [[Base s]] (State fs ts) = let (Tdef _ t2) = head (lookup5 (fs ++ ts) (peel s (State fs ts)))
                                                            in case t2 of
                                                                    (Tpl t) -> tpl_len_comp2 x t (State fs ts)
                                                                    _       -> (State fs ts, False, "")
tpl_len_comp (TupleList (x:xs) _) (t:ts) st = let (state1, valid1, error1) = tpl_len_comp x [t] st
                                                  in if(valid1)
                                                          then tpl_len_comp2 xs ts state1
                                                          else (st, False, error1)
tpl_len_comp _ [] st = (st, False, "")                                                                         

check_args :: Expr -> Def -> State -> (State, Bool, String)
check_args Empty (Fdef s (Sgl Em) t) st = (st, True, "")
check_args Empty (Fdef s _ _) st = (st, False, "\nNo arguments in left side of equation in def of function " ++ s)
check_args _ (Fdef s (Sgl Em) t) st= (st, False, "\nArguments given in left side of equation in def of function " ++ s ++ " that takes no arguments")
check_args (ESymbol s2 _) (Fdef s1 t1 t2) (State fs ts) = if(exists_in (fs ++ ts) s2)
                                                                then (State fs ts, False, "\nBariable name " ++ s2 ++ " has already been used in the scope of function " ++ s1)
                                                                else (State ([Fdef s2 (Sgl Em) t1] ++ fs) ts, True, "")
check_args (ETuple e _) (Fdef s1 (Sgl (Base s2)) t1) (State fs ts) =  let (Tdef _ t2) = head (lookup5 (fs ++ ts) (peel s2 (State fs ts)))
                                                                                in case t2 of
                                                                                        (Tpl tp) -> let (state1, valid1, error1) = tpl_len_comp e tp (State fs ts)
                                                                                                        in if(valid1)
                                                                                                                then (state1, True, "")
                                                                                                                else if(null error1)
                                                                                                                        then ((State fs ts), False, "\nArguments of function " ++ s1 ++ " cannot be matched to function input type " ++ (t_to_s t2))
                                                                                                                        else ((State fs ts), False, error1)
                                                                                        _ -> (State fs ts, False, "\nArguments of function " ++ s1 ++ " cannot be matched to function input type " ++ (t_to_s t2))
                                                                                        
check_args (ETuple e _) (Fdef s1 (Dbl d) t2) (State fs ts ) = (State fs ts, False, "\nArguments of function " ++ s1 ++ " cannot be matched to function input type " ++ (t_to_s (Dbl d)))
check_args (ETuple e _) (Fdef s1 (Tpl t1) t2) (State fs ts) =  let (state1, valid1, error1) = (tpl_len_comp e t1 (State fs ts))
                                                                   in if(valid1)
                                                                        then (state1, True, "")
                                                                        else if(null error1)
                                                                                then ((State fs ts), False, "\nArguments of function " ++ s1 ++ " cannot be matched to function input type " ++ (t_to_s (Tpl t1)))
                                                                                else ((State fs ts), False, error1)

add_col :: Int -> [Bool]
add_col 0 = []
add_col c = [False] ++ (add_col (c-1))

add_row :: Int -> Int -> [[Bool]]
add_row 0 c = []
add_row r c = [add_col c] ++ (add_row (r-1) c) 

form_board :: Def -> [[Bool]]
form_board (Tdef _ (Dbl [Base v1, Base v2])) = add_row (read v1::Int) (read v2::Int)

init_board_update_c :: [Bool] -> Int -> Int -> [Bool]
init_board_update_c [] _ _ = []
init_board_update_c (x:xs) c ind = if(c == 0 || c == ind)
                                        then [True] ++ (init_board_update_c xs c (ind + 1))
                                        else [x] ++ (init_board_update_c xs c (ind + 1))

init_board_update :: [[Bool]] -> Int -> Int -> Int -> [[Bool]]
init_board_update [] _ _ _ = []
init_board_update (x:xs) r c ind = if(r == 0 || r == ind)
                                        then [init_board_update_c x c 1] ++ (init_board_update xs r c (ind + 1))
                                        else [x] ++ (init_board_update xs r c (ind + 1))

check_initial_board_tpl :: Expr -> [[Bool]] -> ([[Bool]], Bool)
check_initial_board_tpl (ETuple (TupleList [TupleValue (ESymbol s1 _) _, TupleValue (ESymbol s2 _) _] _) _) b = if((s1 == "x") && (s2 == "y"))
                                                                                                                        then (init_board_update b 0 0 1, True)
                                                                                                                        else ([], False)
check_initial_board_tpl (ETuple (TupleList [TupleValue (ESymbol s _) _, TupleValue (EInt v1 _) _] _) _) b = if(s == "x")
                                                                                                                then (init_board_update b 0 v1 1, True)
                                                                                                                else ([], False)
check_initial_board_tpl (ETuple (TupleList [TupleValue (EInt v1 _) _, TupleValue (ESymbol s _) _] _) _) b = if(s == "y")
                                                                        then (init_board_update b v1 0 1, True)
                                                                        else ([], False)
check_initial_board_tpl (ETuple (TupleList [TupleValue (EInt v1 _) _, TupleValue (EInt v2 _) _] _) _) b = (init_board_update b v1 v2 1, True)
check_initial_board_tpl _ _ = ([], False)


check_initial_board :: [Equation] -> Def -> [[Bool]] -> Bool -> State -> (NewType, [[Bool]], Bool, String)
check_initial_board [ArrayEquation s t stmt _] (Tdef _ t2) bcheck flag st = let (bcheck2, v) = (check_initial_board_tpl t bcheck)
                                                                                  in if(v)
                                                                                        then let (type1, valid1, error1) = check_stmt stmt st 
                                                                                                 in if(type_comp type1 t2 st)
                                                                                                         then if(flag || and (map and bcheck2))
                                                                                                                then (type1, bcheck2, valid1, error1)
                                                                                                                else (type1, [], False, "\nBoard is not fully initialized " ++ (show bcheck2)) 
                                                                                                         else (Sgl Em, [], False, "\nReturn type of initialize board function " ++ s ++ " is not of type " ++ (t_to_s t2) ++ error1)
                                                                                        else (Sgl Em, [], False, "\nArguments " ++ (show t) ++ " of initialialize board function " ++ s ++ " are not valid")
check_initial_board (x:xs) d1 bcheck _ st = let (type1, bcheck1, valid1, error1) = check_initial_board [x] d1 bcheck True st
                                                   in if(valid1)
                                                        then let (type2, bcheck2, valid2, error2) = check_initial_board xs d1 bcheck1 True st
                                                                 in if(valid2)
                                                                         then if(and (map and bcheck2))
                                                                                 then (type1, bcheck2, True, "")
                                                                                 else (type1, [], False, "\nBoard is not fully initialized " ++ (show bcheck2))                                                                                
                                                                        else (type1, [], valid2, error2)                                                                   
                                                        else (type1, [], valid1, error1) 
                                                

check_func_call :: [Equation] -> Def -> State -> (NewType, Bool, String)
check_func_call [Equation s e stmt _] (Fdef _ t1 t2) (State fs ts) = let (state1, valid1, error1) = check_args e (Fdef s t1 t2) (State fs ts)
                                                                        in if(valid1)
                                                                                then let (type2, valid2, error2) = check_stmt stmt state1
                                                                                        in if(type_comp type2 t2 (State fs ts))
                                                                                                then (Sgl Em, valid2, error2)
                                                                                                else (Sgl Em, False, "\nReturn type of function " ++ s ++ " does not match. Given: " ++ (t_to_s type2) ++ " Expected: " ++ (t_to_s t2) ++ error2) 
                                                                                else (Sgl Em, False, error1)
check_func_call ((Equation s e stmt _):xs) (Fdef _ t1 t2) (State fs ts) = let (state1, valid1, error1) = check_args e (Fdef s t1 t2) (State fs ts)
                                                        in if(valid1)
                                                                then let (type2, valid2, error2) = check_stmt stmt state1
                                                                         (type3, valid3, error3) = check_func_call xs (Fdef s t1 t2) (State fs ts)
                                                                         in if(type_comp type2 t2 (State fs ts))
                                                                                 then (Sgl Em, valid2 && valid3, error2 ++ error3)
                                                                                 else (Sgl Em, False, "\nReturn type of function " ++ s ++ " does not match. Given: " ++ (t_to_s type2) ++ " Expected: " ++ (t_to_s t2) ++ error2 ++ error3) 
                                                                else let (type2, valid2, error2) = check_func_call xs (Fdef s t1 t2) (State fs ts)
                                                                         in (Sgl Em, False, error1 ++ error2)

check_valid_def_types :: State -> (State, Bool, String)
check_valid_def_types (State fs ts) = let (state1, valid1, error1) = (check_type_types ts builtin_state)
                                        in if(valid1) 
                                                then check_func_types fs state1
                                                else (state1, valid1, error1)

check_func_types :: [Def] -> State -> (State, Bool, String)
check_func_types [] st = (st, True, "")
check_func_types (x:xs) st = let (state1, valid1, error1) = (check_func_types_helper x st)
                                 in let (state2, valid2, error2) = check_func_types xs state1
                                        in (state2, valid1 && valid2, error1 ++ error2)

check_func_types_helper :: Def -> State -> (State, Bool, String)
check_func_types_helper (Fdef s ti to) (State fs ts) = let (state1, type1, valid1, error1) = full_type_to_str s ti (State fs ts)
                                                           in let ((State fs2 ts2), type2, valid2, error2) = full_type_to_str s to state1 
                                                                  in if(valid1 && valid2)
                                                                        then (State (fs2 ++ [Fdef s type1 type2]) ts2, True, "")
                                                                        else (State fs2 ts2, False, error1 ++ error2)
full_type_to_str :: String -> NewType -> State -> (State, NewType, Bool, String)
full_type_to_str _ (Sgl Em) st = (st, (Sgl Em), True, "")
full_type_to_str fstr (Sgl (Base s)) (State fs ts) = if(exists_in fs s) 
                                                        then (State fs ts, (Sgl Em), False, "\nInvalid type " ++ s ++ " in function " ++ fstr)
                                                        else if(exists_in ts s)
                                                                then (State fs ts, Sgl (Base s), True, "")
                                                                else (State ([Fdef s (Sgl Em) (Sgl (Sls s))] ++ fs) ts, (Sgl (Sls s)), True, "")
full_type_to_str fstr (Dbl x) st = let (state1, type1, valid1, error1) = full_type_to_str_x_pre fstr x st 
                                        in (state1, Dbl type1, valid1, error1)
full_type_to_str fstr (Tpl ([Tplblk]:xs)) st = let (t1, t2) = tpl_extract xs
                                                   in let(state1, (Tpl x1), valid1, error1) = full_type_to_str fstr (Tpl t1) st
                                                         in let (state2, (Tpl x2), valid2, error2) = full_type_to_str fstr (Tpl t2) st
                                                                in (state2, Tpl ([[Tplblk]] ++ x1 ++ [[Endtpl]] ++ x2), valid1 && valid2, error1 ++ error2)
full_type_to_str fstr (Tpl (x:xs)) st = let (state1, type1, valid1, error1) = full_type_to_str_x_pre fstr x st
                                              in let (state2, (Tpl x2), valid2, error2) = full_type_to_str fstr (Tpl xs) st
                                                     in (state2, Tpl ([type1] ++ x2), valid1 && valid2, error1 ++ error2)
full_type_to_str fstr (Tpl []) st = (st, Tpl [], True, "")

full_type_to_str_x_pre :: String -> [BaseType] -> State -> (State, [BaseType], Bool, String)
full_type_to_str_x_pre fstr [] st = (st, [], True, "")
full_type_to_str_x_pre fstr ((Base s):xs) (State fs ts) = if(exists_in ts s)
                                                                        then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                 in (state1, [Base s] ++ type1, valid1, error1)
                                                                        else if(exists_in fs s)
                                                                                then (State fs ts, [Em], False, "\nValue " ++ s ++ " in definition of function " ++ fstr ++ ". Values cannot be expanded in function definitions")
                                                                                else (State fs ts, [], False, "\nType " ++ s ++ " is undefined in definition of function " ++ fstr)

full_type_to_str_x :: String -> [BaseType] -> State -> (State, [BaseType], Bool, String)
full_type_to_str_x fstr ((Sls s):xs) (State fs ts) = if(exists_in ts s)
                                                                then if(check_base ts s ts) 
                                                                        then (State fs ts, [Em], False, "\nType " ++ s ++ " in definition of function " ++ fstr ++ ". Types can only be expanded by Values")
                                                                        else let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                in (state1, [Base s], valid1, error1)
                                                                else if(exists_in fs s)
                                                                        then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                 in (state1, [Sls s] ++ type1, valid1, error1)
                                                                        else (State ([Fdef s (Sgl Em) (Sgl (Sls s))] ++ fs) ts, [Sls s], True, "")
full_type_to_str_x fstr [] st = (st, [], True, "")


