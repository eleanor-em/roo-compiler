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
import Control.Monad (unless)

compileProgram :: Program -> Either [AnalysisError] [Text]
compileProgram program@(Program _ _ procs) = do
    let (errs, symbols) = getAllSymbols program
    if not (hasMain symbols) then
        Left [AnalysisError 0 0 "main procedure with no parameters missing"]
    else do
        result <- addHeader <$> leftmap (errs <>) (concatEither $ map (compileProc symbols) procs)
        if null errs then
            return $ separate result
        else
            Left errs
    where
        separate  = map (<> "\n")
        addHeader = (["call proc_main", "halt"] <>)

-- Text processing for prettifying generated Oz code
addIndent :: Text -> Text
addIndent "" = ""
addIndent str
    | T.head str == '#' = str
    | otherwise         = "    " <> str

addComment :: Text -> [Text]
addComment str = ["# " <> T.strip str]

addCommentTo :: Text -> [Text] -> [Text]
addCommentTo str = (["# " <> T.strip str] <>)

makeProcLabel :: Text -> Text
makeProcLabel = ("proc_" <>)

compileProc :: RootTable -> Procedure -> Either [AnalysisError] [Text]
compileProc symbols (Procedure _ (ProcHeader (Ident _ procName) _) _ statements) = 
    case Map.lookup procName (rootProcs symbols) of
        Just (_, locals) -> do
            let addCommentAndCompile st = addCommentTo (prettyStatement 0 st) <$> compileStatement symbols locals st
            instrs <- concatEither $ map addCommentAndCompile statements

            let numLocals = Map.size (localSymbols locals)
            let prologue = if numLocals > 0 then ozPushStackFrame numLocals else []
            let epilogue = if numLocals > 0 then ozPopStackFrame  numLocals else []

            -- Load arguments
            let argPrologue = mconcat $ zipWith ozStore (map symLocation (localParams locals))
                                                        (map Register [0..])

            Right $ concat
                [ ["\n" <> makeProcLabel procName <> ":"]
                , addComment "prologue"
                , map addIndent (prologue <> argPrologue <> instrs)
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
-- | write str;
--   Special case to handle string literals.
compileStatement _ _ (SWrite (LocatedExpr _ (EConst (LitString str))))
    = pure (ozWriteString str)

-- | write expr;
compileStatement symbols locals (SWrite expr) = do
    TypedExpr ty expr <- analyseExpression (rootAliases symbols) locals expr
    (result, final) <- runEither (compileExpr expr) (initialBlockState symbols locals)

    let op TInt  = ozWriteInt
        op TBool = ozWriteBool
        op _     = error $ "internal error: attempted to write invalid type `" <> show ty <> "`" in 
        
        return $ blockInstrs final <> op ty result

-- | writeln expr;
compileStatement symbols locals (SWriteLn expr) = do
    instrs <- compileStatement symbols locals (SWrite expr)
    return $ instrs <> ozWriteString "\\n"

-- | lvalue <- expr;
compileStatement symbols locals (SAssign lvalue expr) = do
    TypedExpr ty' expr' <- analyseExpression (rootAliases symbols) locals expr
    
    sym <- analyseLvalue locals lvalue
    let ty = rawSymType sym
    
    if ty /= ty' then
        let err  = "expecting `" <> tshow ty' <> "` on RHS of `<-`, found `" <> tshow ty <> "`"
            note = "`" <> symName sym <> "` declared here:" in
        Left $ errorWithNote (locate expr) err (symPos sym) note
    else do
        (register, postEval) <- runEither (compileExpr expr') (initialBlockState symbols locals)
        (_, final)           <- runEither (storeSymbol register sym) postEval
        return $ blockInstrs final

compileStatement _ locals (SRead lvalue) = do
    sym <- analyseLvalue locals lvalue

    -- Handle the different types of symbols appropriately
    case symType sym of
        ValSymbol TInt  -> return $ ozReadInt (symLocation sym)
        ValSymbol TBool -> return $ ozReadBool (symLocation sym)

        RefSymbol TInt  -> return $ ozReadIntIndirect (symLocation sym)
        RefSymbol TBool -> return $ ozReadBoolIndirect (symLocation sym)

        _ -> let err  = "expecting `integer` or `boolean`, found `" <> tshow (rawSymType sym) <> "`"
                 note = "`" <> symName sym <> "` declared here:" in
            Left $ errorWithNote (locateLvalue lvalue) err (symPos sym) note

compileStatement symbols locals (SCall (Ident pos procName) args) = do
    (targetPos, targetProc) <- unwrapOr (Map.lookup procName $ rootProcs  symbols)
                                        (liftOne $ errorPos pos $
                                            "unknown procedure `" <> procName <> "`")
    let params = localParams targetProc

    -- Check # arguments = # parameters
    unless  (length args == length params)
            (let err  = mconcat
                    [ "`", procName, "` expects ", countWithNoun (length params) "parameter"
                    , " but was given ", tshow (length args) ]
                 note = "`" <> procName <> "` declared here:" in
                     Left $ errorWithNote pos err targetPos note)

    -- Type-check arguments
    typedArgs <- concatEither $ map ((pure <$>) . analyseExpression (rootAliases symbols) locals)
                                    args

    -- Check argument types match parameter types
    let mismatched = filter (\((_, a), b) -> typeof a /= rawSymType b)
                            (zip (enumerate typedArgs) params)

    let reportErr ((i, expr), symbol) = let err  = mconcat [ "in argument: expecting `"
                                                   , tshow $ rawSymType symbol
                                                   , "`, found `"
                                                   , tshow $ typeof expr
                                                   , "`" ]
                                            note = "parameter declared here:"  in
            Left $ errorWithNote (locate $ args !! i) err (symPos symbol) note

    concatEither $ map reportErr mismatched

    -- Compile arguments
    let compileArgs = mapM (compileExpr . innerExp) typedArgs
    -- Reserve registers for the arguments
    (registers, final) <- runEither compileArgs $ BlockState symbols locals [] (length args)
    -- TODO: load address as necessary

    let moves = concatMap (uncurry ozMove) (zip (map Register [0..]) registers)

    return $ blockInstrs final <> moves <> ozCall (makeProcLabel procName)

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
compileExpr (ELvalue lvalue) = loadLvalue lvalue

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

loadLvalue :: Lvalue -> EitherState BlockState Register
loadLvalue (LId (Ident _ name)) = do
    current <- getEither
    case Map.lookup name (localSymbols $ blockLocalSyms current) of
        Just sym -> loadSymbol sym
        Nothing  -> error "internal error: type check failed"

loadLvalue _ = error "loadLvalue: not yet implemented"

loadSymbol :: ProcSymbol -> EitherState BlockState Register
loadSymbol (ProcSymbol (ValSymbol _) location _ _) = do
    register <- useRegister
    addInstrs $ ozLoad register location
    return register

loadSymbol (ProcSymbol (RefSymbol _) location _ _) = do
    ptr <- useRegister
    register <- useRegister
    addInstrs $ ozLoad         ptr location
             <> ozLoadIndirect register ptr
    return register

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

storeLvalue :: Lvalue -> Register -> EitherState BlockState ()
storeLvalue lvalue register = do
    current <- getEither
    addErrorsOr (analyseLvalue (blockLocalSyms current) lvalue) $ \sym ->
        storeSymbol register sym

storeSymbol :: Register -> ProcSymbol -> EitherState BlockState ()
storeSymbol register (ProcSymbol (ValSymbol _) location _ _)
    = addInstrs $ ozStore location register

storeSymbol register (ProcSymbol (RefSymbol _) location _ _) = do
    ptr <- useRegister
    addInstrs $ ozLoad          ptr location
             <> ozStoreIndirect ptr register
