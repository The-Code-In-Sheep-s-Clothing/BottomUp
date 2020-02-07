module TypeChecker where
import Ast
import Data.List

data State       = State [Def] [Def]
                 deriving Show

data Def        = Tdef String Type
                | Fdef String Type
                 deriving Show

data TypeOrError  = Tor (Maybe Type)
                 deriving Show

str_to_type :: String -> Type
str_to_type x = Ptype' (Xtype' (Xtype [Btype x]))

builtin_state :: State 
builtin_state = State [Fdef "A" str_to_type "A", Fdef "B" str_to_type "B"] [Tdef "Bool" (str_to_type "Bool"), Tdef "Int" (str_to_type "Int"), Tdef "Player"  (Ptype' (Xtype' (Xtype [Btype "A", Btype "B"]))), Tdef "Board" (str_to_type "Board"), Tdef "Position" (Ptype' (Ttype' (Ttype [Xtype [Btype "Int"], Xtype [Btype "Int"]])))]

lookup1 :: [Def] -> String -> Bool
lookup1 [] _ = False
lookup1 ((Tdef s2 _):xs) s1 = if(s1 == s2)
                          then True 
                          else lookup1 xs s1
lookup1 ((Fdef s2 _):xs) s1 = if(s1 == s2)
                          then True 
                          else lookup1 xs s1

lookup3 :: [Def] -> String -> Bool
lookup3 d1 s = let (State _ d2) = builtin_state
                  in lookup1 (d1 ++ d2) s

lookup5 :: [Def] -> String -> String
lookup5 [] _ = ""
lookup5 ((Tdef s1 t):ds) s2 = if(s1 == s2)
                                then btype_to_str t
                                else lookup5 ds s2

lookup6 :: [Def] -> String -> [String]
lookup6 [] _ = []
lookup6 ((Tdef s1 t):ds) s2 = if(s1 == s2)
                                then xtype_to_str t 
                                else lookup6 ds s

lookup2 :: [Def] -> String -> [Def]
lookup2 [] _ = []
lookup2 (d:ds) s = if(lookup1 [d] s)
                        then [d] ++ (lookup2 ds s)
                        else (lookup2 ds s) ++ [d]

lookup4 :: [Def] -> [String] -> [String]
lookup4 d [] = []
lookup4 d (x:xs) = if(lookup1 d x)
                        then lookup4 d xs
                        else [x] ++ (lookup4 d xs) 

type_lookup :: [Def] -> String -> Type
type_lookup [(Tdef _ t)] _ = t
type_lookup ((Tdef s1 t):ds) s2 = if(s1 == s2)
                                        then t
                                        else type_lookup ds s2  

btype_to_str :: Type -> String
btype_to_str (Ptype' (Xtype' (Xtype [Btype s]))) = s
btype_to_str _ = ""

xtype_to_str :: Type -> [String]
xtype_to_str (Ptype' (Xtype' (Xtype []))) = []
xtype_to_str (Ptype' (Xtype' (Xtype (b:bs)))) = [b] ++ (xtype_to_str (Ptype' (Xtype' (Xtype bs))))
xtype_to_str _ = []

check_start :: [Stmt] -> (Bool, String) 
check_start [] = (True, "")
check_start x = let (state1, valid1, error1) = check_dup_name x (State [] [])
                        in if(valid1) 
                           then let (state2, valid2, error2) = check_valid_def_types state1
                                in if(valid2)
                                        then check x state2
                                        else (False, error2)
                           else (False, error1)

check_dup_name :: [Stmt] -> State -> (State, Bool, String)
check_dup_name [] st = (st, True, "")
check_dup_name (x:xs) st = let ((State funcs1 types1), valid1, error1) = check_dup_name_stmt x st
                            in let ((State funcs2 types2), valid2, error2) = check_dup_name xs (State funcs1 types1)
                                    in (State (funcs2) (types2), valid1 && valid2, error2 ++ error1)



check_equation_name :: String -> [Equation] -> Bool
check_equation_name s1 [(Equation s2 _ _)] = s1 == s2
check_equation_name s1 ((Equation s2 _ _):xs) = (s1 == s2) && (check_equation_name s1 xs)

check_dup_name_stmt :: Stmt -> State -> (State, Bool, String) 
check_dup_name_stmt (Typedef s t) (State fs ts)  =  if(lookup3 (fs ++ ts) s)
                                                        then (State fs ts, False, "\nType name " ++ s ++ " already used")
                                                        else (State fs ([Tdef s t] ++ ts), True, "")                                                
check_dup_name_stmt (Valdef (Signature s t) e) (State fs ts) = if(lookup3 (fs ++ ts) s)
                                                                then (State fs ts, False, "\nFunction name " ++ s ++ " already used")
                                                                else if(check_equation_name s e)
                                                                        then (State ([Fdef s t] ++ fs) ts, True, "")
                                                                        else (State fs ts, False, "\nFunction " ++ s ++ " has inconsistent naming" )                                                                
check_dup_name_stmt (TypedefFunc s e t) st  = if(not (s == "Board"))
                                            then (st, False, "\nof type not allowed for non board definitions")
                                            else (st, True, "")
check_dup_name_stmt _ st = (st, True, "")



check_valid_def_types :: State -> (State, Bool, String)
check_valid_def_types (State fs ts) = let (state1, valid1, error1) = check_type_types ts builtin_state
                                        in if(valid1) 
                                                then check_func_types fs state1
                                                else (state1, valid1, error1)


check_func_types :: [Def] -> State -> (State, Bool, String)
check_func_types [] (State fs ts) = (State fs ts, True, "")
check_func_types (x:xs) (State fs ts) = let (state1, valid1, error1) = check_func_types xs (State fs ts)
                                 in let (state2, valid2, error2) = check_func_types_helper x state1 ts
                                        in (state2, valid1 && valid2, error1 ++ error2)

check_func_types_helper :: Def -> State -> [Def] -> (State, Bool, String)
check_func_types_helper (Fdef "initialBoard" _) st  d= (st, True, "")                     --Not handling initial board
check_func_types_helper (Fdef s t) (State fs ts) d = let (tarray, valid, error)  = special_type_to_str t d
                                                        in let invTypes = lookup4 ts tarray
                                                            in if(null invTypes)    
                                                                    then if(valid)
                                                                                then (State ([(Fdef s t)] ++ fs) ts, True, "")
                                                                                else (State fs ts, valid, error)
                                                                    else ((State fs ts), False, "\nThe following types " ++ (intercalate " " invTypes) ++ " in the definition of function " ++ s ++ " are undefined" ++ error)

special_type_to_str :: Type -> [Def] -> ([String], Bool, String)
special_type_to_str (Ftype' (Ftype p1 p2)) d= let (str1, valid1, error1) = special_type_to_str_p p1 d
                                                  (str2, vlaid2, error2) = special_type_to_str_p p2 d
                                                  in (str1 ++ str2, valid1 && valid2, error1 ++ error2)
special_type_to_str (Ptype' p) d = special_type_to_str_p p d

special_type_to_str_p :: Ptype -> [Def] -> ([String], Bool, String)
special_type_to_str_p (Xtype' (Xtype [Btype s])) d = let (cnt, str) = special_type_to_str_b (Btype s) d
                                                   in if(cnt == 1)
                                                           then (str, True, "")
                                                           else (str, False, "\n" ++ s ++ " is not a valid function argument type")
special_type_to_str_p (Xtype' x) d = let (cnt, str) = special_type_to_str_x x d 
                                         in if(cnt > 1)
                                             then (str, False, "\n the following types " ++ (intercalate " " str) ++ " are not cross compatible")     
                                                 (str, valid, error)
special_type_to_str_p (Ttype' []) = ([], True, "")
special_type_to_str_p (Ttype' (t:ts)) = let (cnt, str1) = special_type_to_str_x t d
                                            (str2, valid2, error2) = special_type_to_str_p (Ttype' ts) d 
                                            in (str1 ++ str2, valid1 && valid2, error1 ++ error2)

special_type_to_str_x :: Xtype -> [Def] -> (Int, [String])
special_type_to_str_x (Xtype []) d cnt = (0, [])
special_type_to_str_x (Xtype (x:xs)) d cnt = let (cnt1, str1) = special_type_to_str_x (Xtype xs)
                                                in let (cnt2, str2) = special_type_to_str_b x d
                                                        in (cnt1 ++ cnt2, str1 ++ str2)

special_type_to_str_b :: Btype -> [Def] -> (Int, [String])
special_type_to_str_b (Btype s) d = if(lookup1 d s)
                                        then (1, [])
                                        else (0, [s])


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


ast_convert :: [String] -> Type -> [Def] -> State -> (Type, [Def], State, Bool, String)
ast_convert s (Ptype' (Xtype' x)) d st = let (type1, defs1, (State fs ts), valid1, error1) = ast_convert_x s x d st
                                                in (str_to_type (head s), defs1, (State fs (([Tdef (head s) type1]) ++ ts)), valid1, error1)
ast_convert s (Ptype' (Ttype' t)) d st = let (type1, defs1, (State fs ts), valid1, error1) = ast_convert_t s t d st
                                             in (str_to_type (head s), defs1, (State fs (([Tdef (head s) type1]) ++ ts)), valid1, error1)
                                        

ast_convert_t :: [String] -> Ttype -> [Def] -> State -> (Type, [Def], State, Bool, String)
ast_convert_t s (Ttype [x]) d st = ast_convert_x s x d st
ast_convert_t s (Ttype (x:xs)) d st = let (type1, defs1, state1, valid1, error1) = ast_convert_x s x d st 
                                        in let (type2, defs2, state2, valid2, error2) = ast_convert_t s (Ttype xs) defs1 state1
                                                in (type_unwrapper (tuple_concat (Tor (Just type1)) (Tor (Just type2))), defs2, state2, valid1 && valid2, error1 ++ error2)

ast_convert_x :: [String] -> Xtype -> [Def] -> State -> (Type, [Def], State, Bool, String)
ast_convert_x s (Xtype [b]) d st = ast_convert_b s b d True st
ast_convert_x s (Xtype (b:bs)) d st = let (type1, defs1, state1, valid1, error1) = ast_convert_b s b d False st 
                                        in let (type2, defs2, state2, valid2, error2) = ast_convert_x2 s (Xtype bs) defs1 state1
                                                in (type_unwrapper (type_concat (Tor (Just type1)) (Tor (Just type2))), defs2, state2, valid1 && valid2, error1 ++ error2)

ast_convert_x2 :: [String] -> Xtype -> [Def] -> State -> (Type, [Def], State, Bool, String)
ast_convert_x2 s (Xtype [b]) d st = ast_convert_b s b d False st
ast_convert_x2 s (Xtype (b:bs)) d st = let (type1, defs1, state1, valid1, error1) = ast_convert_b s b d False st 
                                        in let (type2, defs2, state2, valid2, error2) = ast_convert_x2 s (Xtype bs) defs1 state1
                                                in (type_unwrapper (type_concat (Tor (Just type1)) (Tor (Just type2))), defs2, state2, valid1 && valid2, error1 ++ error2)

ast_convert_b :: [String] -> Btype -> [Def] -> Bool -> State -> (Type, [Def], State, Bool, String)
ast_convert_b s1 (Btype s2) d singleFlag (State fs ts) = if(lookup1 ts s2)
                                                                then (str_to_type (type_down s2 ts), d, state fs ts, True, "")
                                                                else if(lookup1 fs s2)
                                                                        then if(singleFlag)
                                                                                (str_to_type "error", d, State fs ts, False, "\nyou really shouldn't do that")
                                                                        else if(lookup1 d s2) 
                                                                                then let ((Tdef s3 t2):dfs) = lookup2 d s2
                                                                                        in ast_convert ([s3] ++ s1) t2 dfs (State fs ts)
                                                                                else if(and (map ((==) s2) s1))
                                                                                        then (str_to_type "error", d, State fs ts, False, "\nType definition of type " ++ s2 ++ " cannot refrence the following types " ++ (intercalate " " s2))
                                                                                        else if(singleFlag)
                                                                                                then (str_to_type "error", d, State fs ts, False, "\nyou really shouldn't do that")
                                                                                                else (str_to_type s2, d, State ([Fdef (str_to_type s2)] ++ fs) ts, True, "") 
                                                        

type_down :: String -> [Def] -> String
type_down s d = let s2 = lookup5 d s 
                    in if(null s2)
                            then s
                            else type_down d s2


check :: [Stmt] -> State -> (Bool, String)
check [] _ = (True, "")
check (x:xs) st = let (state1, valid1, error1) = check_stmt x st
                      (valid2, error2) = check xs st
                        in (valid1 && valid2, error1 ++ error2)

type_or_error_check :: TypeOrError -> Bool
type_or_error_check (Tor Nothing) = False
type_or_error_check _ = True 

type_unwrapper :: TypeOrError -> Type
type_unwrapper (Tor (Just t)) = t

check_stmt :: Stmt -> State -> (TypeOrError, Bool, String)
check_stmt (Conditional e stmt1 stmt2) st = let (type1, valid1, error1) = check_expr e st
                                                (type2, valid2, error2) = check_stmt stmt1 st
                                                (type3, valid3, error3) = check_stmt stmt2 st
                                                in if((compare_root_type "Bool" type1))
                                                    then if(condition_type_match type2 type3 st)
                                                        then (clean_dup (type_concat type2 type3) st, valid1 && valid2 && valid3, error1 ++ error2 ++ error3)
                                                    else (Tor Nothing, False, "\nExpression " ++ (show e) ++ " is not of type Bool" ++ error1 ++ error2 ++ error3)
check_stmt (Let s e stmt) (State fs ts)           = let (type1, valid1, error1) = check_expr e (State fs ts)
                                                        in if(type_or_error_check type1)
                                                                then let (type2, valid2, error2) = check_stmt stmt (State ([(Tdef s (type_unwrapper type1))] ++ fs) ts )
                                                                        in (type2, valid2, error2)
                                                                else (Tor Nothing, valid1, error1)
check_stmt (While e1 e2) st          = let (type1, valid1, error1) = check_expr e1 st
                                           (type2, valid2, error2) = check_expr e2 st
                                        in if((compare_root_type "Bool" type1))
                                            then (type2, valid1 && valid2, error1 ++ error2)
                                            else (Tor Nothing, False, "\nExpression " ++ (show e1) ++ " is not of type Bool" ++ error1 ++ error2)
check_stmt (Valdef (Signature s t) e) st          = let (valid, error) = check_equations e t st
                                                        in (Tor Nothing, valid, error)
check_stmt (Typedef s t) st           = (Tor Nothing, True, "")
check_stmt (TypedefFunc s e t) st      = (Tor Nothing, True, "")
check_stmt (SExpr e) st               = check_expr e st

condition_type_match :: TypeOrError -> TypeOrError -> State -> (TypeOrError, Bool, String)
condition_type_match (Tor Nothing) _ _= (Tor Nothing, False, "")
condition_type_match _ (Tor Nothing) _= (Tor Nothing, False, "")
condition_type_match (Tor (Just (Ptype' (Xtype' x1)))) (Tor (Just (Ptype' (Xtype' x2)))) st = let (xtp, valid) = condition_type_match_x x1 x2 st 
                                                                                                  in (Tor (Just (Ptype' (Xtype' xtp))), valid, if(valid) then "" else "\n condition stmt types don't match") 
condition_type_match (Tor (Just (Ptype' (Ttype' (Ttype (x1:xs1)))))) (Tor (Just (Ptype' (Ttype' (Ttype (x2:xs2)))))) st = let (xtp1, valid1) = condition_type_match_x x1 x2 st 
                                                                                                                              (Tor (Just (Ptype' (Ttype' (Ttype t)))), valid2, error2) = condition_type_match (Tor (Just (Ptype' (Ttype' (Ttype xs1))))) (Tor (Just (Ptype' (Ttype' (Ttype xs2))))) st
                                                                                                                              in (Tor (Just (Ptype' (Ttype' (Ttype (xtp1 ++ t))))), valid1 && valid2, if(valid1 && valid2) then "" else "\ncondition stmts don't have matching types")
condition_type_match (Tor (Just (Ptype' (Ttype' (Ttype []]))))) (Tor (Just (Ptype' (Ttype' (Ttype []]))))) = (Tor (Just (Ptype' (Ttype' (Ttype []])))), True, "")
condition_type_match _ _ _ = False

deconstruct :: Xtype-> (Btype, Btype)
deconstruct (Xtype x) = let (b1, b2) = pull_xtype 

condition_type_match_x :: Xtype -> Xtype -> State -> (Xtype, Bool)
condition_type_match_x (Xtype x1) (Xtype x2) st = let (str1, str2) = (pull_xtype x1 st, pull_xtype st)
                                                      in if(str1 == str2 || null str1 || null str2)
                                                        then True
                                                        else (go_deeper str1 str2 x2 st) || (go_deeper str2 str1 x1 st)

pull_xtype :: [Btypes] -> State -> Btypes
pull_xtype [] st = ""
pull_xtype ((Btype s):bs) (State fs ts) = if(lookup1 ts s)
                                                then s
                                                else pull_xtype bs (State fs ts)

remove_dup :: [Btype]

go_deeper :: String -> String -> [Btype] -> [Btype] -> State -> ([Btype], Bool)
go_deeper s s st = True
go_deeper s1 s2 b st = let s3 = lookup6 ts s1
                           in if(and (map ((==) s1) s3))
                                then ([], False)
                                else if(even_deeper s3 b)
                                        then s3 == s2
                                        else go_deeper s3 s2 (State fs ts)

even_deeper :: [String] -> [Btype] -> [Btype]
even_deeper (s:ss) (b:bs) = 

clean_dup :: TypeOrError -> State -> TypeOrError
clean_dup (Tor Nothing) st = Tor Nothing
clean_dup (Tor (Just (Ptype' (Ttype' (Ttype x))))) st = Tor (Just ( Ptype' (Ttype' (Ttype (map (clean_dup_x [] st) x)))))
clean_dup (Tor (Just (Ptype' (Xtype' x)))) st = Tor (Just (Ptype' (Xtype' (clean_dup_x [] st x))))

clean_dup_x :: [Btype] -> State -> Xtype -> Xtype
clean_dup_x _ _ (Xtype []) = Xtype []
clean_dup_x bs st (Xtype x:xs) = let b = clean_dup_b bs st x
                                     in clean_dup_x b st (Xtype xs)

clean_dup_b :: [Btype] -> State -> Btype -> [Btype]
clean_dup_b [] _ b -> [b]
clean_dup_b ((Btype s1):bs) st (Btype s2) = if(s1 == s2)
                                                then (Btype s1):(Btype s2):bs
                                                else (Btype s1):bs



check_arg_expr2 :: Expr -> [Btype]
check_arg_expr2 (Paren e) = check_arg_expr2 e st
check_arg_expr2 (Tuple x)  = Ttype'' (Ttype [(Xtype (map check_arg_expr2 x))])
check_arg_expr2 (ESymbol s)  = Btype s
check_arg_expr2 (Empty) -> []

check_arg_expr1 :: Expr -> [Xtype]
check_arg_expr1 (Paren e) = check_arg_expr1 e st
check_arg_expr1 (Tuple x)  = [Xtype (map check_arg_expr2 x)]
check_arg_expr1 (ESymbol s)  = [Xtype [Btype s]]
check_arg_expr1 (Empty) -> []

check_equations :: [Equation] -> Type -> State -> (Bool, String)
check_equations [x] t st = check_equation x t st
check_equations (x:xs) t st = let (valid1, error1) = check_equations xs t st
                                  (valid2, error2) = check_equation x t st
                                  in (valid1 && valid2, error1 ++ error2)

set_args :: [Xtype] -> [Xtype] -> [Def] -> [Def]
set_args [Xtype []] [Xtype []] d = d
set_args [Xtype ((Btype s1):b1s)] [Xtype ((Btype s2):b2s)] d = [Fdef s1 (Ptype' (Xtype' (Xtype [Btype s2])))] ++ (set_args (Xtype [b1s]) (Xtype [b2s]))
set_args [Xtype ((Ttype'' (Ttype x)):b1s)] [Xtype ((Btype s2):b2s)] d

check_equation :: Equation -> Type -> State -> (Bool, String)
check_equation (Equation "initialBoard" _ _) _ _ = (True, "")
check_equation (Equation s e stmt) t (State fs ts)   = let vars = check_arg_expr1 e 
                                                           (args, return) = parse_func_type t
                                                           in if((length vars) == (length args))
                                                                then let (type2, valid2, error2) = check_stmt stmt (State (set_args vars args fs) ts)
                                                                        in if(arg_type_compare type2 (Tor (Just return))) 
                                                                                then (valid1 && valid2, error1 ++ error2)
                                                                                else (False, "\nReturn type on function " ++ s ++ " doesn't match")
                                                                else (False, "\nfunction " ++ s ++ "has incorrect number of arguments")
                                                                        
                                                        

parse_func_type :: Type -> ([Xtype], [Xtype])
parse_func_type (Ptype' p) = ([], parse_func_type_p p2)
parse_func_type (Ftype' (Ftype p1 p2)) = (parse_func_type_p p1, parse_func_type_p p2)

parse_func_type_p :: Ptype -> [Xtype]
parse_func_type_p (Ttype' (Ttype x)) = x
parse_func_type_p (Xtype' x) = [x] 

parse_func_type_t (Ttype )

type_concat :: TypeOrError -> TypeOrError -> TypeOrError
type_concat (Tor Nothing) t = Tor Nothing
type_concat t (Tor Nothing) = Tor Nothing
type_concat (Tor (Just (Ptype' (Xtype' (Xtype b1))))) (Tor (Just (Ptype' (Xtype' (Xtype b2))))) = Tor (Just (Ptype' (Xtype' (Xtype (b2 ++ b1)))))
type_concat (Tor (Just (Ptype' (Ttype' t)))) (Tor (Just (Ptype' (Xtype' (Xtype x))))) = Tor (Just (Ptype' (Xtype' (Xtype ([Ttype'' t] ++ x)))))
type_concat (Tor (Just (Ptype' (Xtype' (Xtype x))))) (Tor (Just (Ptype' (Ttype' t)))) = Tor (Just (Ptype' (Xtype' (Xtype ([Ttype'' t] ++ x)))))
type_concat (Tor (Just (Ptype' (Ttype' t1)))) (Tor (Just (Ptype' (Ttype' t2)))) = Tor (Just (Ptype' (Xtype' (Xtype ([Ttype'' t1, Ttype'' t2])))))

tuple_concat :: TypeOrError -> TypeOrError -> TypeOrError
tuple_concat (Tor Nothing) _ = Tor Nothing
tuple_concat _ (Tor Nothing) = Tor Nothing
tuple_concat (Tor (Just (Ptype' (Xtype' x1)))) (Tor (Just (Ptype' (Xtype' x2)))) = Tor (Just (Ptype' (Ttype' (Ttype [x1, x2]))))
tuple_concat (Tor (Just (Ptype' (Ttype' t)))) (Tor (Just (Ptype' (Xtype' x)))) = Tor (Just (Ptype' (Ttype' (Ttype ([Xtype [Ttype'' t], x])))))
tuple_concat (Tor (Just (Ptype' (Xtype' x)))) (Tor (Just (Ptype' (Ttype' t)))) = Tor (Just (Ptype' (Ttype' (Ttype ([x, Xtype [Ttype'' t]] )))))
tuple_concat (Tor (Just (Ptype' (Ttype' t1)))) (Tor (Just (Ptype' (Ttype' t2)))) = Tor (Just (Ptype' (Ttype' (Ttype ([Xtype [Ttype'' t1], Xtype [Ttype'' t2]] )))))




check_expr :: Expr -> State -> (TypeOrError, Bool, String)
check_expr (EInt _) st                          = (Tor (Just (str_to_type "int")), True, "")
check_expr (Paren e) st                         = check_expr e st
check_expr (Tuple t) st                         = check_tuple t st
check_expr (Infix e1 b e2) st                   = check_binop e1 b e1 st
check_expr (FunctionApp s e) (State fs ts)      = check_function_app s e fs (State fs ts) 
check_expr (ESymbol s) (State fs ts)            = check_symbol s (fs ++ ts)
check_expr (Empty) st                           = (Tor (Just (str_to_type "")), True, "")

check_function_app :: String -> Expr -> [Def] -> State -> (TypeOrError, Bool, String)
check_function_app s1 e [Fdef s2 t] st = if(s1 == s2)
                                            then check_function_args s1 e (Fdef s2 t) st
                                            else (Tor Nothing, False, "\n No function with name " ++ s1) 
check_function_app s1 e ((Fdef s2 t):xs) st = if(s1 == s2)
                                                then check_function_args s1 e (Fdef s2 t) st
                                                else check_function_app s1 e xs st


check_function_args :: String -> Expr -> Def -> State -> (TypeOrError, Bool, String)
check_function_args s1 e (Fdef s2 (Ftype' (Ftype p1 p2))) st = let (type1, valid1, error1) = check_expr e st
                                                                in if(valid1)
                                                                    then if(type_compare type1 (Tor (Just (Ptype' p1))))
                                                                        then (Tor (Just (Ptype' p2)), True, "") 
                                                                        else (Tor Nothing, False, "\nType of arguments " ++ (show e) ++ "does not match expected type of function " ++ s1)
                                                                    else (Tor Nothing, False, error1)


arg_type_compare :: TypeOrError -> TypeOrError -> Bool
arg_type_compare (Tor (Just (Ftype' (Ftype p1 p2)))) t = type_compare (Tor (Just (Ptype' p1))) t
arg_type_compare t (Tor (Just (Ftype' (Ftype p1 p2)))) = type_compare (Tor (Just (Ptype' p1))) t
arg_type_compare t1 t2 = type_compare t1 t2 

type_compare :: TypeOrError -> TypeOrError -> Bool
type_compare (Tor (Just (Ftype' (Ftype p1 p2)))) t = type_compare (Tor (Just (Ptype' p2))) t
type_compare t (Tor (Just (Ftype' (Ftype p1 p2)))) = type_compare (Tor (Just (Ptype' p2))) t
type_compare (Tor (Just (Ptype' p1))) (Tor (Just (Ptype' p2))) = ptype_compare p1 p2
type_compare (Tor Nothing) (Tor Nothing) = True
type_compare _ _ = False

ptype_compare :: Ptype -> Ptype -> Bool
ptype_compare (Xtype' x1) (Xtype' x2) = xtype_compare x1 x2 
ptype_compare (Ttype' t1) (Ttype' t2) = ttype_compare t1 t2
ptype_compare _ _ = False

ttype_compare :: Ttype -> Ttype -> Bool
ttype_compare (Ttype []) _ = False
ttype_compare _ (Ttype []) = False
ttype_compare (Ttype [x1]) (Ttype [x2]) = xtype_compare x1 x2
ttype_compare (Ttype (x1:xs1)) (Ttype (x2:xs2)) = if(xtype_compare x1 x2)
                                                      then ttype_compare (Ttype xs1) (Ttype xs2)
                                                      else False

xtype_compare :: Xtype -> Xtype -> Bool
xtype_compare (Xtype []) _ = False
xtype_compare _ (Xtype []) = False
xtype_compare (Xtype [b1]) (Xtype [b2]) = btype_compare b1 b2
xtype_compare (Xtype (b1:b1s)) (Xtype (b2:b2s)) = if(btype_compare b1 b2)
                                                      then True
                                                      else xtype_compare (Xtype b1s) (Xtype b2s)

btype_compare :: Btype -> Btype -> Bool
btype_compare (Btype s1) (Btype s2) = s1 == s2
btype_compare (Ttype'' t1) (Ttype'' t2) = ttype_compare t1 t2
btype_compare _ _ = False

check_symbol :: String -> [Def] -> (TypeOrError, Bool, String)
check_symbol s [x] =  check_symbol2 s x
check_symbol s (x:xs) = let (type1, valid1, error1) = check_symbol2 s x
                        in if(valid1) 
                            then (type1, valid1, error1)
                            else check_symbol s xs

check_symbol2 :: String -> Def -> (TypeOrError, Bool, String)
check_symbol2 s1 (Fdef s2 (Ptype' p)) = if(s1 == s2)
                                              then (Tor (Just (Ptype' p)), True, "")
                                              else (Tor Nothing, False, "\n" ++ s1 ++ " is undefined")
check_symbol2 s1 (Fdef s2 (Ftype' _)) = if(s1 == s2)
                                              then (Tor Nothing, False, "\n" ++ s1 ++ " is lacking arguments")
                                              else (Tor Nothing, False, "\n" ++ s1 ++ " is undefined")
check_symbol2 s1 ((Tdef s2 t))               = if(s1 == s2)
                                              then (Tor (Just t), True, "")
                                              else (Tor Nothing, False, "\n" ++ s1 ++ " is undefined")

check_tuple :: [Expr] -> State -> (TypeOrError, Bool, String)
check_tuple [x] st    = let (type1, valid1, error1) = check_expr x st
                                in (type1, valid1, error1)
check_tuple (x:xs) st = let (type1, valid1, error1) = check_expr x st
                            (type2, valid2, error2) = check_tuple xs st
                            in (tuple_concat type1 type2, valid1 && valid2, error1 ++ error2)

check_binop :: Expr -> Binop -> Expr -> State -> (TypeOrError, Bool, String)
check_binop e1 Plus e2 st             = check_binop_help e1 e2 "PLUS" (Tor (Just (str_to_type "Int"))) st
check_binop e1 Minus e2 st            = check_binop_help e1 e2 "Minus" (Tor (Just (str_to_type "Int"))) st
check_binop e1 Times e2 st            = check_binop_help e1 e2 "Times" (Tor (Just (str_to_type "Int"))) st  
check_binop e1 Div e2 st              = check_binop_help e1 e2 "Div" (Tor (Just (str_to_type "Int"))) st 
check_binop e1 LessThan e2 st         = check_binop_help e1 e2 "LessThan" (Tor (Just (str_to_type "Bool"))) st 
check_binop e1 GreaterThan e2 st      = check_binop_help e1 e2 "GreaterThan" (Tor (Just (str_to_type "Bool"))) st
check_binop e1 LessThanEqual e2 st    = check_binop_help e1 e2 "LessThanEqual" (Tor (Just (str_to_type "Bool"))) st
check_binop e1 GreaterThanEqual e2 st = check_binop_help e1 e2 "GreaterThanEqual" (Tor (Just (str_to_type "Bool"))) st
check_binop e1 EqualTo e2 st          = let (type1, valid1, error1) = check_expr e1 st                                    --Outstanding 
                                            (type2, valid2, error2) = check_expr e2 st
                                        in if(type_compare type1 type2)
                                            then (Tor (Just (str_to_type "Bool")), valid1 && valid2, error1 ++ error2)
                                            else (Tor Nothing, False, "\n" ++ (show type1) ++ " " ++ (show type2) ++ " are invalid args for function ==" ++ error1 ++ error2)                            

check_binop_help :: Expr -> Expr -> String -> TypeOrError -> State -> (TypeOrError, Bool, String)
check_binop_help e1 e2 s t st = let (type1, valid1, error1) = check_expr e1 st
                                    (type2, valid2, error2) = check_expr e2 st
                                    in if((compare_root_type "Int" type1) && (compare_root_type "Int" type2))
                                      then (t, valid1 && valid2, error1 ++ error2)
                                      else (Tor Nothing, False,  "\n" ++ (show type1) ++ " " ++ (show type2) ++ " are invalid args for function " ++ s ++ error1 ++ error2)

compare_root_type :: String -> TypeOrError -> Bool
compare_root_type _ (Tor Nothing) = True
compare_root_type s (Tor (Just (Ptype' p)))  = compare_root_ptype s p 
compare_root_type s (Tor (Just (Ftype' (Ftype _ p)))) = (compare_root_ptype s p)

compare_root_ptype :: String -> Ptype -> Bool
compare_root_ptype s (Xtype' x) = compare_root_xtype s x
compare_root_ptype s _ = False

compare_root_xtype :: String -> Xtype -> Bool
compare_root_xtype s (Xtype x) = or (map (compare_root_btype s) x) 

compare_root_btype :: String -> Btype -> Bool
compare_root_btype s1 (Btype s2) = s1 == s2
compare_root_btype _ _ = False
