module RooCompile where

import Common
import RooAnalyse
import RooAst
import SymTable

compileProgram :: Program -> Either [AnalysisError] [String]
compileProgram program@(Program _ arrays procs) = do
    -- Symbol table analysis
    symbols <- getAllSymbols program
    seq (unsafePrintSymbols symbols) $ do
        ozProcs <- combineErrors [] $ map compileProc procs
        return $ concat [ozProcs]

compileProc :: Procedure -> Either [AnalysisError] [String]
compileProc (Procedure _ _ _ statements) = do
    ozStatements <- combineErrors [] $ map (compileStatement 0) statements
    return $ concat [ozStatements]

compileStatement :: Int -> Statement -> Either [AnalysisError] [String]
compileStatement rIndex (SWrite expr) = do
    typed <- analyseExpression expr
    return $ []

compileStatement _ _ = error "not yet implemented"
