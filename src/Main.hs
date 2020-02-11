module Main where
import Parser
import Lexer
import Compiler
import Ast
-- import TypeChecker

type Env = String -> [Stmt]
emptyEnv = error "Not found"
envLookup s env = env s
envBind s v env = (\s' -> if s == s' then v else env s)
{-
eval :: Exp -> Env -> Int
eval (Int v) _         = v
eval (Plus e1 e2) env  = (eval e1 env) + (eval e2 env)
eval (Minus e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Times e1 e2) env = (eval e1 env) * (eval e2 env)
eval (Div e1 e2) env   = (eval e1 env) `div` (eval e2 env)
eval (Negate e) env    = -(eval e env)
eval (Var s) env       = eval (envLookup s env) env
eval (Let s e1 e2) env = eval e2 env'
    where env' = envBind s e1 env
    
run :: Exp -> Int
run e = eval e emptyEnv
-}
main :: IO ()
main = do
    s <- getContents
    let parsedEither = parse s

    case parsedEither of
        Left s -> putStrLn s
        Right ast -> do
            print ast
            writeFile "output.hs" (compile ast)
    
    -- let (valid, error) = check_start(ast)
    -- putStrLn error
   
    --print (run ast)
    