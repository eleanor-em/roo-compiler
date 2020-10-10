{-# LANGUAGE OverloadedStrings #-}

module RooCompile where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Common

import RooAnalyse
import RooAst
import RooPrettyPrinter
import SymTable
import Oz

compileProgram :: Program -> Either [AnalysisError] [Text]
compileProgram program@(Program _ _ procs) = do
    let (errs, symbols) = getAllSymbols program
    if not (hasMain symbols) then
        Left [AnalysisError 0 0 "main procedure with no parameters missing"]
    else do
        result <- addHeader <$> mapErr (errs <>) (concatEither $ map (compileProc symbols) procs)
        Right $ separate result
    where
        separate  = map (<> "\n")
        addHeader = (["call proc_main", "halt", ""] <>)

-- Text processing for prettifying generated Oz code
-- `strip` exists in Data.Text but then we'd have to use Text the whole time...

addIndent :: Text -> Text
addIndent "" = ""
addIndent str
    | T.head str == '#' = str
    | otherwise         = "    " <> str

addComment :: Text -> [Text]
addComment str = ["# " <> T.strip str]

compileProc :: RootTable -> Procedure -> Either [AnalysisError] [Text]
compileProc symbols (Procedure _ (ProcHeader (Ident _ procName) _) _ statements) = 
    case Map.lookup procName (rootProcs symbols) of
        Just (_, locals) -> do
            instrs <- concatEither $ map (compileStatement symbols locals) statements
            let numLocals = Map.size (localSymbols locals)

            let prologue = if numLocals > 0 then ozPushStackFrame numLocals else []
            let epilogue = if numLocals > 0 then ozPopStackFrame  numLocals else []

            Right $ concat
                [ ["proc_" <> procName <> ":"]
                , addComment "prologue"
                , map addIndent (prologue <> instrs)
                , addComment "epilogue"
                , map addIndent (epilogue <> ["return"]) ]

        Nothing  -> error "internal error: missed a procedure somehow"

data BlockState = BlockState
    { blockRootSyms :: RootTable
    , blockLocalSyms :: LocalTable
    , blockInstrs :: [Text]
    , blockNextReg :: Int }

initialBlockState :: RootTable -> LocalTable -> BlockState
initialBlockState symbols locals = BlockState symbols locals [] 0

compileStatement :: RootTable -> LocalTable -> Statement -> Either [AnalysisError] [Text]
-- Special-case string write
compileStatement _ _ st@(SWrite (LocatedExpr _ (EConst (LitString str))))
    = Right $ addComment (prettyStatement 0 st) <> ozWriteString str

compileStatement symbols locals (SWrite expr) = do
    TypedExpr ty expr <- analyseExpression (rootAliases symbols) locals expr
    (result, final) <- runEither (compileExpr expr) (initialBlockState symbols locals)

    let op TInt  = ozWriteInt
        op TBool = ozWriteBool
        op _     = error $ "internal error: attempted to write invalid type `" <> show ty <> "`" in 
        
        return $ blockInstrs final <> op ty result

compileStatement symbols locals st@(SWriteLn expr) = do
    let comment = addComment $ prettyStatement 0 st
    instrs <- compileStatement symbols locals (SWrite expr)
    return $ comment <> instrs <> ozWriteString "\\n"

compileStatement symbols locals st@(SAssign lvalue expr) = do
    let comment = addComment $ prettyStatement 0 st
    TypedExpr ty expr <- analyseExpression (rootAliases symbols) locals expr
    (register, postEval) <- runEither (compileExpr expr) (initialBlockState symbols locals)
    -- TODO: check that the lvalue type matches (otherwise can assign bools to ints)
    (_, final) <- runEither (storeLValue lvalue register) postEval
    return $ comment <> blockInstrs final

compileStatement _ _ _ = error "compileStatement: not yet implemented"

useRegister :: EitherState BlockState Register
useRegister = do
    current <- getEither
    let register = blockNextReg current
    putEither (current { blockNextReg = register + 1})
    return $ Register register

addInstrs :: [Text] -> EitherState BlockState ()
addInstrs instrs = do
    current <- getEither
    let prevInstrs = blockInstrs current
    putEither (current { blockInstrs = prevInstrs <> instrs})

compileExpr :: Expression -> EitherState BlockState Register
compileExpr (ELvalue lvalue) = loadLValue lvalue

compileExpr (EConst lit) = loadConst lit

compileExpr (EUnOp op expr) = do
    result <- useRegister
    eval <- compileExpr (fromLocated expr)
    addInstrs (ozOp result eval)
    return result

    where
        ozOp = case op of
            UnNot    -> ozNot
            UnNegate -> ozNeg

compileExpr (EBinOp op lexp rexp) = do
    result <- useRegister
    lhs <- compileExpr (fromLocated lexp)
    rhs <- compileExpr (fromLocated rexp)
    addInstrs (ozOp result lhs rhs)
    return result

    where
        ozOp = case op of
            BinOr    -> ozOr
            BinAnd   -> ozAnd
            BinEq    -> ozEq
            BinNeq   -> ozNeq
            BinLt    -> ozLt
            BinLte   -> ozLte
            BinGt    -> ozGt
            BinGte   -> ozGte
            BinPlus  -> ozPlus
            BinMinus -> ozMinus
            BinTimes -> ozTimes
            BinDivide -> ozDivide

loadLValue :: LValue -> EitherState BlockState Register
loadLValue (LId (Ident _ name)) = do
    current <- getEither
    case Map.lookup name (localSymbols $ blockLocalSyms current) of
        Just sym -> loadSymbol sym
        Nothing  -> error "internal error: type check failed"

loadLValue _ = error "loadLValue: not yet implemented"

loadSymbol :: ProcSymbol -> EitherState BlockState Register
loadSymbol (ProcSymbol (ValSymbol _) location _) = do
    register <- useRegister
    addInstrs $ ozLoad register location
    return register

loadSymbol _ = error "loadSymbol RefSymbol: not yet implemented"

loadConst :: Literal -> EitherState BlockState Register
loadConst (LitBool val) = do
    register <- useRegister
    addInstrs $ ozBoolConst register val
    return register

loadConst (LitInt val) = do
    register <- useRegister
    addInstrs $ ozIntConst register val
    return register

loadConst _ = error "internal error: tried to load Text constant"

storeLValue :: LValue -> Register -> EitherState BlockState ()
storeLValue (LId (Ident _ name)) register = do
    current <- getEither
    case Map.lookup name (localSymbols $ blockLocalSyms current) of
        Just sym -> storeSymbol register sym
        Nothing  -> error "internal error: type check failed"

storeLValue _ _ = error "storeLValue: not yet implemented"

storeSymbol :: Register -> ProcSymbol -> EitherState BlockState ()
storeSymbol register (ProcSymbol (ValSymbol _) location _)
    = addInstrs $ ozStore location register

storeSymbol _ _ = error "storeSymbol RefSymbol: not yet implemented"
