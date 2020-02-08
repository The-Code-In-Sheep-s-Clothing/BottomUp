module TypeChecker where
import Ast
import Data.List

data State       = State [Def] [Def]
                 deriving Show

data Def        = Tdef String NewType
                | Fdef String NewType Newtype
                 deriving Show

data NewType = Sgl BaseType
             | Dbl [BaseType]
             | Tpl [[BaseType]] [BaseType]
             | Em 
             deriving Show
data BaseType  = Base String 
               | Sls String
               | Ifblk
               deriving Show

ftype_convert :: String -> Type -> Def
ftype_convert s (Ftype' p1 p2) = Fdef s (type_convert (Ptype' p1)) (type_convert (Ptype' p2))

type_convert :: Type -> NewType
type_convert (Ptype' (Ttype' t)) = let (x, b) = ttype_convert t
                                       in Tpl x b
type_convert (Ptype' (Xtype' x)) = Dbl xtype convert x 

ttype_convert :: Ttype -> ([[BaseType]], [BaseType])
ttype_convert (Ttype []) = ([[]], [])
ttype_convert (Ttype ([x]:xs)) = let b = btype_convert x
                                     (t, d) = ttype_convert (Ttype xs)
                                     in (t, (d ++ [b]))
ttype_convert (Ttype (x:xs)) = let b = xtype_convert x
                                   (t, d) = ttype_convert (Ttype xs)
                                   in ((t ++ [b]), t)

xtype_convert :: Xtype -> [BaseType]
xtype_convert (Xtype b) = map btype_convert b

btype_convert :: Btype -> BaseType
btype_convert (Btype s) = Base s

exist_in_sls :: [Def] -> String -> Bool
exists_in_sls [] _ = False
exists_in_sls ((Tdef s2 (Sgl (Sls _))):xs) s1 = if(s1 == s2)
                                                then True 
                                                else exists_in_sls xs s1
exists_in_sls ((Fdef s2 (Sgl (Sls _))):xs) s1 = if(s1 == s2)
                                                then True 
                                                else exists_in_sls xs s1
exist_in_sls (x:xs) s = exists_in_sls xs s

exists_in :: [Def] -> String -> Bool
exists_in [] _ = False
exists_in ((Tdef s2 _):xs) s1 = if(s1 == s2)
                                        then True 
                                        else exists_in xs s1
exists_in ((Fdef s2 _):xs) s1 = if(s1 == s2)
                                        then True 
                                        else exists_in xs s1

lookup_start :: [Def] -> String -> Bool
lookup_start d1 s = let (State _ d2) = builtin_state
                  in exists_in (d1 ++ d2) s

lookup2 :: [Def] -> String -> [Def]
lookup2 [] _ = []
lookup2 (d:ds) s = if(exists_in [d] s)
                        then [d] ++ (lookup2 ds s)
                        else (lookup2 ds s) ++ [d]

builtin_state :: State 
builtin_state = State  [Fdef "A" Em (Sgl (Sls "A")), 
                        Fdef "A" Em (Sgl (Sls "B"))] 
                       [Tdef "Bool" (Sgl (Base "Bool")), 
                        Tdef "Int" (Sgl (Base "Int")), 
                        Tdef "Player" (Tple [] [Sls "A", Sls "B"]), 
                        Tdef "Board" (Sgl (Base "Board")), 
                        Tdef "Position" (Tbl [] [Base "Int", Base "Int"])]

check_start :: [Stmt] -> (Bool, String) 
check_start [] = (True, "")
check_start x = let (state1, valid1, error1) = check_dup_name x (State [] [])
                        in if(valid1) 
                           then let (state2, valid2, error2) = check_valid_def_types state1
                                in (valid2, error2)
                                        --then check x state2
                                        --else (False, error2)
                           else (False, error1)

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
check_func_types_helper (Fdef "initialBoard" _ _) st  d = (st, True, "")                     --Not handling initial board
check_func_types_helper (Fdef s ti to) (State fs ts) = let (state1, type1, valid1, error1) = full_type_to_str s ti (State fs ts)
                                                           in let (state2, type2, valid2, error2) = full_type_to_str s to state1 
                                                        let  (tarray, valid, error)  = special_type_to_str t d ts
                                                        in let invTypes = lookup4 ts tarray
                                                            in if(null invTypes)    
                                                                    then if(valid)
                                                                                then (State ([(Fdef s t)] ++ fs) ts, True, "")
                                                                                else (State fs ts, valid, error)
                                                                    else ((State fs ts), False, "\nThe following types " ++ (intercalate " " invTypes) ++ " in the definition of function " ++ s ++ " are undefined" ++ error)

full_type_to_str :: String -> NewType -> State -> (State, NewType, Bool, String)
full_type_to_str fstr (Sgl (Base s)) (State fs ts) = if(exists_in_sls fs s) 
                                                        then (State fs ts, False, "\n Dataless type is not a valid single arg")
                                                        else if(exists_in ts s)
                                                                then (State fs ts, Sgl (Base s), True, "")
                                                                else (State fs ts, Em, False, "\n Type " ++ s ++ " in function " ++ fstr ++ " is undefined")
full_type_to_str fstr (Dbl x) st = let (state1, type1, valid1, error1) = full_type_to_str_x_pre fstr x st 
                                        in (state1, Dbl type1, valid1, error1)
full_type_to_str fstr (Tpl (x:xs) b) st = let (state1, type1, valid1, error1) = full_type_to_str_x_pre fstr x st
                                              in let (state2, (Tpl x2 b2), valid2, error2) = full_type_to_str fstr (Tpl xs b) st
                                                     in (state2, Tpl ([type1] ++ x2) b2, valid1 && valid2, error1 && error2)
full_type_to_str fstr (Tpl [] b) st = let (state1, type1, valid1, error1) = full_type_to_str_x_pre fstr b st
                                          in (state1, Tpl ([] b), valid1, error1)
full_type_to_str fstr (Tpl [] []) st = (st, Tpl [] [], True, "")

full_type_to_str_x_pre :: String -> [BaseType] -> State -> (State, [BaseType], Bool, String)
full_type_to_str_x_pre fstr (Dbl ((Base s):xs)) (State fs ts) = if(exists_in ts s)
                                                                        then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                 in (state1, [Base s] ++ type1, valid1, error1)
                                                                        else if(exist_in_sls fs s)
                                                                                then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                         in (state1, [Sls s] ++ type1, valid1, error1)
                                                                                else if(exists_in fs s)
                                                                                        then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                                 in (state1, [], False, "\n in definition of function " ++ fstr ++ " name " ++ s ++ " already in use" ++ error1)
                                                                                        else let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State (fs ++ [Fdef s (Sgl (Sls s))]) ts)
                                                                                                 in (state1, [Sls s] ++ type1, valid1, error1)

full_type_to_str_x :: String -> [BaseType] -> State -> (State, [BaseType], Bool, String)
full_type_to_str_x fstr ((Base s):xs) (State fs ts) = if(exists_in ts s)
                                                                then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                         in (state1, [], False, "\n value having type only allowed in first arg of |: " ++ s ++ " in function " ++ fstr ++ error1)
                                                                else if(exist_in_sls fs s)
                                                                        then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                 in (state1, [Sls s] ++ type1, valid1, error1)
                                                                        else if(exists_in fs s)
                                                                                then let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State fs ts)
                                                                                         in (state1, [], False, "\n in definition of function " ++ fstr ++ " name " ++ s ++ " already in use" ++ error1)
                                                                                else let (state1, type1, valid1, error1) = full_type_to_str_x fstr xs (State (fs ++ [Fdef s (Sgl (Sls s))]) ts)
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
ast_convert s (Dbl x) d st = let (type1, defs1, (State fs ts), valid1, error1) = ast_convert_x s x d st
                                                in (Sgl (Base (head s)), defs1, (State fs (([Tdef (head s) type1]) ++ ts)), valid1, error1)
ast_convert s (Tpl x b) d st = let (Tpl x1 b1, defs1, (State fs1 ts1), valid1, error1) = ast_convert_t s x d st
                                   in let ((Dbl b2), defs2, (State fs2 ts2), valid2, error2) = ast_convert_x s b d (State fs1 ts1) 
                                             in (Sgl (Base (head s)), defs1, (State fs (([Tdef (head s) (Tpl x1 b2)]) ++ ts)), valid1, error1)
                                        

ast_convert_t :: [String] -> [[BaseType]] -> [Def] -> State -> (NewType, [Def], State, Bool, String)
ast_convert_t s (Tple []) d st = (Tple [] [], d, st, True, "")
ast_convert_t s (Tple (x:xs)) d st = let (Dbl x1, defs1, state1, valid1, error1) = ast_convert_x s x d st 
                                        in let (Tpl x2 b2, defs2, state2, valid2, error2) = ast_convert_t s (Ttype xs) defs1 state1
                                                in (Tpl (x2 ++ x1) b1, defs2, state2, valid1 && valid2, error1 ++ error2)

ast_convert_x :: [String] -> [BaseType] -> [Def] -> State -> (NewType, [Def], State, Bool, String)
ast_convert_x s ([b]) d st = ast_convert_b s b d True st
ast_convert_x s (b:bs) d st = let ((Sgl base1), defs1, state1, valid1, error1) = ast_convert_b s b d False st 
                                        in let (Dbl base2, defs2, state2, valid2, error2) = ast_convert_x2 s bs defs1 state1
                                                in (Dbl [base1] ++ base2, defs2, state2, valid1 && valid2, error1 ++ error2)

ast_convert_x2 :: [String] -> [BaseType] -> [Def] -> State -> (NewType, [Def], State, Bool, String)
ast_convert_x2 s (Xtype [b]) d st = ast_convert_b s b d False st
ast_convert_x2 s (Xtype (b:bs)) d st = let (type1, defs1, state1, valid1, error1) = ast_convert_b s b d False st 
                                        in let (type2, defs2, state2, valid2, error2) = ast_convert_x2 s (Xtype bs) defs1 state1
                                                in (type_unwrapper (type_concat (Tor (Just type1)) (Tor (Just type2))), defs2, state2, valid1 && valid2, error1 ++ error2)

ast_convert_b :: [String] -> BaseType -> [Def] -> Bool -> State -> (NewType, [Def], State, Bool, String)
ast_convert_b s1 (Sls s2) d _ (State fs ts) = if(exists_in fs s2)
                                                                then (Sgl (Sls s2), d, State fs ts, True, "")
                                                                else if(exists_in ts s2)
                                                                        then (Em, d, State fs ts, False, "\n Value containing types can only be used on the left side of |")
                                                                        else (Sgl (Sls s2), d, State ([Fdef s2 (Sgl (Sls s2))] ++ fs) ts, True, "")
ast_convert_b s1 (Base s2) d singleFlag (State fs ts) = if(exists_in ts s2)
                                                                then (Sgl (Sls s2), d, State fs ts, True, "")
                                                                else if(exists_in fs s2)
                                                                        then if(singleFlag)
                                                                                then (Em, d, State fs ts, False, "\nyou really shouldn't")
                                                                                else (Sgl (Sls s2), d, State fs ts, True, "")
                                                                        else (exists_in d s2)
                                                                                then let ((Tedef s3 t2):dfs) = lookup_2 d s2
                                                                                        in ast_convert ([s3] ++ s1) t2 dfs (State fs ts)
                                                                                else if(and (map ((==) s2) s1))
                                                                                        then (Em, d, State fs ts, False, "\nType definition of type " ++ s2 ++ " cannot refrence the following types " ++ (intercalate " " s2))
                                                                                        else if(singleFlag)
                                                                                                then (Em, d, State fs ts, False, "\nyou really shouldn't do that")
                                                                                                else (Sgl (Base s2), d, State ([Fdef s2 (Sgl (Sls s2))] ++ fs) ts, True, "")  
