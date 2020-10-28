{-|
Module      : Main
Description : Main program for the compiler

Main program written by Eleanor McMurtry & Jenny Yan for Programming Language Implementation
Assignment 3.
-}

-- Needed to make colour printing behave
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Console.GetOpt
import System.Directory (doesFileExist)

import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T

import Text.Parsec (runParser)

import Common
import RooParser (pProgram, ParsedAst)
import RooPrettyPrinter (prettyPrint)
import RooCompile (verifyProgram, compileProgramFragment, compileProgram)
import System.IO (hPrint, stderr)

import RooPreprocessor
import Data.Maybe (mapMaybe)
import RooAst

-- | Represents the various command-line arguments
data Flag = GenAst | PrettyPrint | TestPrettyPrinter | Help
    deriving Show

-- | Specifying the compiler flag options including the flag, flag arguments
--   if required, and default if erroneous arguments are provided
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
        (_, _, errs) -> fail (concat errs <> "\n" <> usage)

-- | Handling the command-line flag that was read and performing the appopriate action
handleAst :: Flag -> ParsedAst -> IO ()
handleAst GenAst ast = do
    print ast
    exitSuccess

handleAst PrettyPrint ast = do
    T.putStr $ prettyPrint ast
    exitSuccess

handleAst TestPrettyPrinter ast = do
    let prettyPrinted = prettyPrint ast
    let reparsed = runParser pProgram 0 "" (unpack prettyPrinted)
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

getAst :: [String] -> IO (ParsedAst, [String])
getAst progNames =
    if null progNames then do
        putStrLn "error: must provide file"
        putStr usage
        exitFailure
    else do
        let progName = head progNames
        fileExists <- doesFileExist progName
        if not fileExists then do
            putStrLn $ "error: file `" <> progName <> "` does not exist"
            exitFailure
        else do
            input <- readFile progName

            let output = runParser pProgram 0 "" (removeIncludes input)
            case output of
                Right ast -> return (ast, lines input)
                Left  err -> do
                    hPrint stderr err
                    exitFailure

data ConsoleCol = White | Green | Blue | Red | Yellow | Reset

addCol :: ConsoleCol -> Text
addCol White  = "\x1b[1;37m"
addCol Green  = "\x1b[1;32m"
addCol Blue   = "\x1b[1;34m"
addCol Red    = "\x1b[1;31m"
addCol Yellow = "\x1b[1;33m"
addCol Reset  = "\x1b[1;0m"

location :: Int -> Int -> String
location 0 0  = ""
location line col = ":" <> show line <> ":" <> show col

isWarning :: AnalysisError -> Bool
isWarning AnalysisWarn {} = True
isWarning _ = False

label :: String -> [String] -> Int -> Int -> Text -> Text -> IO ()
label progName raw line col err typeline = do
    T.hPutStrLn stderr $ mconcat
        [ addCol White
        , pack $ progName <> location line col <> ": "
        , typeline
        , err ]
    when (line > 0) $ T.hPutStrLn stderr $ mconcat
        [ pack $ raw !! (line - 1) <> "\n"
        , pack $ take (col - 1) $ cycle " "
        , addCol Green <> "^" <> addCol Reset]

labelError :: String -> [String] -> AnalysisError -> IO ()
labelError progName raw (AnalysisError line col err)
    = label progName raw line col err (addCol Red <> "error: " <> addCol Reset)

labelError progName raw (AnalysisNote line col err)
    = label progName raw line col err (addCol Blue <> "note: " <> addCol Reset)

labelError progName raw (AnalysisWarn line col err)
    = label progName raw line col err (addCol Yellow <> "warning: " <> addCol Reset)

reportErrors :: String -> [String] -> [AnalysisError] -> IO ()
reportErrors progName raw = mapM_ (labelError progName raw)

-- | Main function of Roo. Grab the flags, and print a usage message if incorrect
-- or if the help flag is specified. Otherwise, pass arguments to the meat of the
-- program.
main :: IO ()
main = do
    args <- getArgs
    (flags, progNames) <- compilerFlags args
    if null flags then do
        (ast@(Program recs arrs procs), raw) <- getAst progNames
        -- At this point, progNames is known to be non-empty
        let progName = head progNames
        let filenameLen = length $ reverse (takeWhile (/= '/') (reverse progName))
        let path = reverse (drop filenameLen (reverse progName))

        -- Parse includes
        let includeFiles = map (path <>) (concat $ mapMaybe extractIncludes raw)
        (asts, raws) <- unzip <$> mapM (getAst . pure) includeFiles

        -- Syntax-check the includes
        let errLists = map (fst . compileProgramFragment) asts
        mapM_ (uncurry3 reportErrors)
              (zip3 includeFiles raws errLists)

        -- Syntax-check the file itself
        reportErrors progName raw (verifyProgram ast asts)

        -- Compile the final program
        let unpackProgram = (\(Program recs arrs procs) -> (recs, arrs, procs))
        let (recs', arrs', procs') = unzip3 $ map unpackProgram  asts
        let finalAst = Program (recs <> concat recs')
                               (arrs <> concat arrs')
                               (procs <> concat procs')
        let (errs, output) = compileProgram finalAst

        if null errs || all isWarning errs then do
            T.putStr $ mconcat output
            exitSuccess
        else
            exitFailure
    else case head flags of
        Help -> do
            putStr usage
            exitFailure
        flag -> do
            (ast, _) <- getAst progNames
            handleAst flag ast
