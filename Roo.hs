{-|
Module      : Main
Description : Main program for the compiler  

Main program written by $team for Programming Language Implementation 
Assignment 1b. This program handles command-line flags and calls the appropriate function to 
handle the request.  
-}
module Main where 
import System.Environment
import System.Exit
import System.Console.GetOpt 
import System.Directory
import Text.Parsec
import Text.Pretty.Simple (pPrint)

import RooParser
import RooPrettyPrinter

-- | Represents the various command-line arguments
data Flag = GenAst | PrettyPrint | TestPrettyPrinter | Help
    deriving Show 

-- | Specifying the compiler flag options including the flag, flag arguments if required, and
-- default if erroneous arguments are provided
options :: [OptDescr Flag] 
options =
    [ Option ['a'] ["ast"]    (NoArg GenAst)            "generate AST"
    , Option ['p'] ["pretty"] (NoArg PrettyPrint)       "pretty print"
    , Option ['T'] ["test"]   (NoArg TestPrettyPrinter) "test pretty printer"
    , Option ['h'] ["help"]   (NoArg Help)              "display usage info" ]

-- | Generates a usage message for Roo flags 
usage :: String
usage = usageInfo header options
    where header = "Usage: Roo [OPTIONS] [FILE]"

-- | Reading and returning a list of command-line flags and commandline arguments 
compilerFlags :: [String] -> IO ([Flag], [String])
compilerFlags argv =
    case getOpt RequireOrder options argv of 
        (o, n, []) -> return (o, n)
        (_, _, errs) -> fail (concat errs ++ "\n" ++ usage)
    
-- | Handling the command-line flag that was read and performing the appopriate action
handleAst :: Flag -> ParsedAst -> IO ()
handleAst GenAst ast = do
    pPrint ast
    exitSuccess

handleAst PrettyPrint ast = do
    putStr $ prettyPrint ast
    exitSuccess

handleAst TestPrettyPrinter ast = do
    let prettyPrinted = prettyPrint ast
    let reparsed = runParser pProgram 0 "" prettyPrinted
    case reparsed of 
        Right ast' -> do
            let rePrettyPrinted = prettyPrint ast'
            if prettyPrinted == rePrettyPrinted then do
                putStrLn "OK."
                exitSuccess
            else do
                putStrLn "Failed test case!"
                exitFailure
        Left err -> do
            putStrLn "error re-parsing pretty printed code:"
            print err
            exitFailure

-- Below should never happen
handleAst Help _ = undefined

-- | Takes a list of program filenames, checks if it's non-empty, and if so
-- runs the appropriate sub-command on the program filename.
runProgram :: Flag -> [String] -> IO ()
runProgram flag progNames =
    if length progNames == 0 then do
        putStrLn "error: must provide file"
        putStr usage
        exitFailure
    else do
        let progName = head progNames
        fileExists <- doesFileExist progName
        if not fileExists then do
            putStrLn $ "error: file `" ++ progName ++ "` does not exist"
            exitFailure
        else do
            input <- readFile progName

            let output = runParser pProgram 0 "" input
            case output of 
                Right ast -> handleAst flag ast
                Left  err -> do
                    print err
                    exitFailure

-- | Main function of Roo. Grab the flags, and print a usage message if incorrect
-- or if the help flag is specified. Otherwise, pass arguments to the meat of the
-- program.
main :: IO ()
main = do
    args <- getArgs
    (flags, progNames) <- compilerFlags args 
    if length flags == 0 then do
        putStr usage
        exitFailure
    else case head flags of
        Help -> do
            putStr usage
            exitFailure
        flag -> runProgram flag progNames
