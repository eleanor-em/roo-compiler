module RooCompile where

import qualified Data.Map.Strict as Map

import Common
import RooAnalyse
import RooAst
import SymTable

compileProgram :: Program -> Either [AnalysisError] [String]
compileProgram program@(Program _ arrays procs) = do
    -- Symbol table analysis
    symbols <- getAllSymbols program
    if not $ hasMain (rootProcs symbols) then
        Left [AnalysisError 0 0 "main procedure with no parameters missing"]
    else do
        -- seq (unsafePrintSymbols symbols) $ do
        ozProcs <- combineErrors [] $ map (compileProc symbols) procs
        return $ concat [ozProcs]

compileProc :: RootTable -> Procedure -> Either [AnalysisError] [String]
compileProc symbols (Procedure _ (ProcHeader (Ident _ procName) _) _ statements) = do
    case Map.lookup procName (rootProcs symbols) of
        Just (_, locals) -> do
            ozStatements <- combineErrors [] $ map (compileStatement symbols locals 0) statements
            return $ concat [ozStatements]
        Nothing -> error "invalid state: missed a procedure somehow"

compileStatement :: RootTable -> LocalTable -> Int -> Statement -> Either [AnalysisError] [String]
compileStatement symbols locals regIndex (SWrite expr) = do
    typed <- analyseExpression (rootAliases symbols) locals expr
    return $ []

compileStatement _ _ _ _ = error "compileStatement: not yet implemented"
