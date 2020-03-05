module Main where
import Parser
import Lexer
import Compiler
import Ast
--import TypeChecker
import Control.Exception

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
    let parsedEitherFile = parse $ file
    let parsedEitherPrelude = parse $ prelude
    print parsedEitherFile
    
    case parsedEitherFile of
        Left err -> putStrLn "Error in boardgame code" >> putStrLn err
        Right ast -> do
            print ast
            --let (valid, error) = check_start ast
                --in if(valid) 
                        --then   
            writeFile "OutputCode.hs" (compile ast) >> writeFile "OutputBuiltins.hs" (compile_builtin ast)
                        --else   putStrLn error   
    case parsedEitherPrelude of
        Left err -> putStrLn "Error in prelude" >> putStrLn err
        Right ast -> do
            print ast
            --let (valid, error) = check_start ast
                --in if(valid) 
                        --then   
            appendFile "OutputBuiltins.hs" ("\n\n -- Prelude \n" ++ 
                    compile_prelude ast)
                        --else   putStrLn error

    -- let (valid, error) = check_start(ast)
    -- putStrLn error
   
    --print (run ast)
    
