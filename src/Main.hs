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
    
