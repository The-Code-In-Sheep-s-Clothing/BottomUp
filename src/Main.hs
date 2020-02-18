module Main where
import Parser
import Lexer
import Compiler
import TypeChecker
import Ast
import Control.Exception
-- import TypeChecker

type Env = String -> [Stmt]
emptyEnv = error "Not found"
envLookup s env = env s
envBind s v env = (\s' -> if s == s' then v else env s)

getFileContent :: String -> FilePath -> IO String
getFileContent def filePath = readFile filePath `catch`
    \e -> const (return def) (e :: IOException)

main :: IO ()
main = do
    file <- getContents
    prelude <- getFileContent "" "prelude.bgl"
    let parsedEither = parse $ file ++ "\n\n\n-- PRELUDE\n" ++ prelude

    case parsedEither of
        Left err -> putStrLn err
        Right ast -> do
            print ast
            let (valid, error) = check_start ast 
            putStr error
            --writeFile "output.hs" (compile ast)
    
    -- let (valid, error) = check_start(ast)
    -- putStrLn error
   
    --print (run ast)
    
