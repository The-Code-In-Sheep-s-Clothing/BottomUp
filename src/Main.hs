module Main where
import Parser
import Lexer
import Compiler
import Ast
import TypeChecker
import Control.Exception
import System.Environment   

type Env = String -> [Stmt]
emptyEnv = error "Not found"
envLookup s env = env s
envBind s v env = (\s' -> if s == s' then v else env s)

getFileContent :: String -> FilePath -> IO String
getFileContent def filePath = readFile filePath `catch`
    \e -> const (return def) (e :: IOException)

main :: IO ()
main =
    -- read commmand line arguments
    getArgs >>= \args ->
    if length args /= 1 
    then
        putStrLn "Error: Please provide exactly one file to compile"
    else do 
        file <- getFileContent "" $ args !! 0
        prelude <- getFileContent "" "prelude.bgl"
        let parsedEitherFile = parse $ file
        let parsedEitherPrelude = parse $ prelude
        
        case parsedEitherPrelude of
            Left err1 -> putStrLn "Error in prelude" >> putStrLn err1
            Right ast1 -> case parsedEitherFile of
                            Left err2 -> putStrLn "Error in boardgame code" >> putStrLn err2
                            Right ast2 -> let (state1, valid1, error1) = check_start_prelude ast1 in 
                                            if(valid1) 
                                                then let (valid2, error2) = check_start ast2 state1 in 
                                                         if(valid2) 
                                                            then do 
                                                                writeFile "OutputCode.hs" (compile ast2) >> writeFile "OutputBuiltins.hs" (compile_builtin ast2)
                                                                appendFile "OutputBuiltins.hs" ("\n\n -- Prelude \n" ++ compile_prelude ast1)
                                                            else putStrLn error2
                                                else   putStrLn error1 

        -- let (valid, error) = check_start(ast)
        -- putStrLn error
    
        --print (run ast)