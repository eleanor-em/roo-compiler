module RooCompile where

import Control.Monad.State
import qualified Data.Map.Strict as Map

import Common
import RooAnalyse
import RooAst
import SymTable
import Oz

compileProgram :: Program -> Either [AnalysisError] [String]
compileProgram program@(Program _ _ procs) = do
    -- Symbol table analysis
    symbols <- getAllSymbols program
    if not $ hasMain symbols then
        Left [AnalysisError 0 0 "main procedure with no parameters missing"]
    else do
        -- seq (unsafePrintSymbols symbols) $ do
        let results = map (compileProc symbols) procs
        ozProcs <- combineErrorsWith (flip (++)) [] results 
        return $ concat [ozProcs]

compileProc :: RootTable -> Procedure -> Either [AnalysisError] [String]
compileProc symbols (Procedure _ (ProcHeader (Ident _ procName) _) _ statements) = do
    case Map.lookup procName (rootProcs symbols) of
        Just (_, locals) -> do
            ozStatements <- runEitherState (mapM (compileStatement symbols locals) statements) (BlockState [] [] 0)
            -- ozStatements <- combineErrorsWith (flip (++)) [] results
            return $ ozStatements
        Nothing -> error "internal error: missed a procedure somehow"

data BlockState = BlockState
    { blockErrors :: [AnalysisError]
    , blockInstrs :: [String]
    , blockNextReg :: Int }

instance EitherState [String] BlockState where
    stateResult st
        | length (blockErrors st) > 0 = Left (blockErrors st)
        | otherwise = Right (blockInstrs st)

compileStatement :: RootTable -> LocalTable -> Statement -> State BlockState ()
compileStatement symbols locals (SWrite expr) = do
    current <- get
    let prevErrs = blockErrors current

    case analyseExpression (rootAliases symbols) locals expr of
        Left errs -> do
            put (current { blockErrors = prevErrs ++ errs })
        Right _   -> do
            return ()

compileStatement _ _ _ = error "compileStatement: not yet implemented"

useRegister :: State BlockState Int
useRegister = do
    current <- get
    let register = blockNextReg current
    put (current { blockNextReg = register + 1})
    return register

addInstrs :: [String] -> State BlockState ()
addInstrs instrs = do
    current <- get
    let prevInstrs = blockInstrs current
    put (current { blockInstrs = prevInstrs ++ instrs})

compileConst :: Literal -> State BlockState ()
compileConst (LitBool val) = do
    register <- useRegister
    addInstrs $ ozBoolConst register val

compileConst (LitInt val) = do
    register <- useRegister
    addInstrs $ ozIntConst register val

compileConst _ = error "internal error: tried to load string constant"
