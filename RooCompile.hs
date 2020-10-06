module RooCompile where

import qualified Data.Map.Strict as Map

import Common
import RooAnalyse
import RooAst
import SymTable
import Oz

compileProgram :: Program -> Either [AnalysisError] [String]
compileProgram program@(Program _ _ procs) = do
    let (errs, symbols) = getAllSymbols program
    if not (hasMain symbols) then
        Left [AnalysisError 0 0 "main procedure with no parameters missing"]
    else do
        let (errs', result) = concatPair $ map (compileProc symbols) procs
        let finalErrs = errs <> errs'
        if not (null finalErrs) then
            Left finalErrs
        else
            Right result

compileProc :: RootTable -> Procedure -> ([AnalysisError], [String])
compileProc symbols (Procedure _ (ProcHeader (Ident _ procName) _) _ statements) = do
    case Map.lookup procName (rootProcs symbols) of
        Just (_, locals) -> do
            let compile = mapM_ (compileStatement symbols locals) statements
            let (errs, _) = execEither compile (BlockState [] 0)
            
            (errs, [])
        Nothing ->
            error "internal error: missed a procedure somehow"

data BlockState = BlockState
    { blockInstrs :: [String]
    , blockNextReg :: Int }

compileStatement :: RootTable -> LocalTable -> Statement -> EitherState BlockState ()
compileStatement symbols locals (SWrite expr)
    = addErrorsOr (analyseExpression (rootAliases symbols) locals expr) $ \_ -> do
        return ()

compileStatement _ _ _ = error "compileStatement: not yet implemented"

useRegister :: EitherState BlockState Int
useRegister = do
    current <- getEither
    let register = blockNextReg current
    putEither (current { blockNextReg = register + 1})
    return register

addInstrs :: [String] -> EitherState BlockState ()
addInstrs instrs = do
    current <- getEither
    let prevInstrs = blockInstrs current
    putEither (current { blockInstrs = prevInstrs <> instrs})

compileConst :: Literal -> EitherState BlockState ()
compileConst (LitBool val) = do
    register <- useRegister
    addInstrs $ ozBoolConst register val

compileConst (LitInt val) = do
    register <- useRegister
    addInstrs $ ozIntConst register val

compileConst _ = error "internal error: tried to load string constant"
