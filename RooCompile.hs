module RooCompile where

import RooAnalyse
import RooAst

compileProgram :: Program -> Either [AnalysisError] [String]
compileProgram (Program _ _ procs) = do
    ozProcs <- combineErrors [] $ map compileProc procs
    return $ concat [ozProcs]

compileProc :: Procedure -> Either [AnalysisError] [String]
compileProc (Procedure _ _ statements) = do
    ozStatements <- combineErrors [] $ map compileStatement statements
    return $ concat [ozStatements]

compileStatement :: Statement -> Either [AnalysisError] [String]
compileStatement (SWrite expr) = do
    typed <- analyseExpression expr
    return $ []

compileStatement _ = error "not yet implemented"