{-# LANGUAGE OverloadedStrings #-}

module RooCompile where

import Data.Maybe (catMaybes)
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

            let stackSize = localStackSize locals
            let prologue = if stackSize > 0 then ozPushStackFrame stackSize else []
            let epilogue = if stackSize > 0 then ozPopStackFrame  stackSize else []

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
    case result of
        Just result -> 
            let op TInt  = ozWriteInt
                op TBool = ozWriteBool
                op _     = error $ "internal error: attempted to write invalid type `" <> show ty <> "`" in 
            return $ blockInstrs final <> op ty result

        _ -> return []

-- | writeln expr;
compileStatement symbols locals (SWriteLn expr) = do
    instrs <- compileStatement symbols locals (SWrite expr)
    return $ instrs <> ozWriteString "\\n"

-- | lvalue <- expr;
compileStatement symbols locals (SAssign lvalue expr) = do
    -- TODO: check ty' is of primitive type
    TypedExpr ty' expr' <- analyseExpression (rootAliases symbols) locals expr
    sym <- analyseLvalue (rootAliases symbols) locals lvalue
    let ty = lvalueType sym

    case ty of
        TArray _ _ -> typeError ty
        TRecord _  -> typeError ty
        _ -> do    
            if ty /= ty' then
                let err  = "expecting `" <> tshow ty' <> "` on RHS of `<-`, found `" <> tshow ty <> "`"
                    note = "`" <> lvalueName sym <> "` declared here:" in
                Left $ errorWithNote (locate expr) err (lvaluePos sym) note
            else do
                (register, postEval) <- runEither (compileExpr expr') (initialBlockState symbols locals)
                case register of
                    Just register -> do
                        (_, final) <- runEither (storeSymbol register sym) postEval
                        return $ blockInstrs final
                    _ -> return []
    where
        typeError ty = liftOne $ errorPos (locateLvalue lvalue) $
            "expected variable of primitive type on LHS of `<-`, found `" <> tshow ty <> "`" 

compileStatement symbols locals (SRead lvalue) = do
    sym <- analyseLvalue (rootAliases symbols) locals lvalue
    let ty = lvalueType sym

    readInstrs <- case ty of
        TInt  -> Right ozReadInt
        TBool -> Right ozReadBool
        _     -> do
            -- TODO: error with note
            let err  = "expecting `integer` or `boolean` after `read`, found `" <> tshow ty <> "`" in
                liftOne $ errorPos (lvaluePos sym) err
    (_, final) <- runEither (storeLvalue lvalue (Register 0))
                                (BlockState symbols locals readInstrs 1)
    return $ blockInstrs final

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

    -- Check reference args are filled with lvalues
    let mismatched = filter (\((_, a), b) -> not (checkRefArgs a b))
                            (zip (enumerate typedArgs) params)

    let reportErr ((i, _), symbol) = let err  = mconcat [ "in argument: expecting lvalue" ]
                                         note = "parameter declared here:" in
            Left $ errorWithNote (locate $ args !! i) err (symPos symbol) note

    concatEither $ map reportErr mismatched

    -- Compile arguments
    let typedArgs' = zip typedArgs params
    (registers, final) <- runEither (mapM compileArg typedArgs') $ BlockState symbols locals [] (length args)

    let moves = concatMap (uncurry ozMove)
                          (zip (map Register [0..]) (catMaybes registers))

    return $ blockInstrs final <> moves <> ozCall (makeProcLabel procName)

    where
        -- | Checks whether the expression and symbol are either:
        --   lvalue + ref
        --   any + val
        checkRefArgs (TypedExpr _ (ELvalue _)) (ProcSymbol (RefSymbol _) _ _ _) = True
        checkRefArgs _ (ProcSymbol (RefSymbol _) _ _ _) = False
        checkRefArgs _ _ = True

        -- | Compile the argument, loading the address for ref types and the value for val types
        compileArg (TypedExpr _ (ELvalue lvalue), ProcSymbol (RefSymbol _) _ _ _)
            = loadAddress lvalue
        compileArg (TypedExpr _ expr, _)
            = compileExpr expr

-- --TODO: Handling the if and while statements  
-- compileStatement symbols locals (SIf expr statements) = do 
--     TypedExpr ty expr <- analyseExpression (rootAliases symbols) locals 
--     -- the type of the expression checked must be boolean. 
--     -- body must be well type statements 
    

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

compileExpr :: Expression -> EitherState BlockState (Maybe Register)
compileExpr (ELvalue lvalue) = loadLvalue lvalue

compileExpr (EConst lit) = Just <$> loadConst lit

compileExpr (EUnOp op expr) = do
    result <- useRegister
    eval <- compileExpr (fromLocated expr)
    case eval of
        Just eval -> do
            addInstrs (ozOp result eval)
            return $ Just result
        _ -> return Nothing        

    where
        ozOp = case op of
            UnNot    -> ozNot
            UnNegate -> ozNeg

compileExpr (EBinOp op lexp rexp) = do
    result <- useRegister
    lhs <- compileExpr (fromLocated lexp)
    rhs <- compileExpr (fromLocated rexp)
    case (lhs, rhs) of
        (Just lhs, Just rhs) -> do
            addInstrs (ozOp result lhs rhs)
            return $ Just result
        _ -> return Nothing

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

loadLvalue :: Lvalue -> EitherState BlockState (Maybe Register)
loadLvalue lvalue = do
    current <- getEither
    case analyseLvalue (rootAliases $ blockRootSyms current) (blockLocalSyms current) lvalue of
        Left errs -> do
            addErrors errs
            return Nothing
        Right sym -> loadSymbol sym


loadAddress :: Lvalue -> EitherState BlockState (Maybe Register)
loadAddress lvalue = do
    current <- getEither
    case analyseLvalue (rootAliases $ blockRootSyms current) (blockLocalSyms current) lvalue of
        Left errs -> do
            addErrors errs
            return Nothing

        Right (TypedRefLvalue _ location offset _ _) ->
            if noOffset == offset then do
                -- Copy the reference to the target
                register <- useRegister
                addInstrs $ ozLoad register location
                return $ Just register
            else do
                -- Copy the reference to the target and add offset
                baseReg <- useRegister
                offsetReg <- compileExpr offset
                case offsetReg of
                    Just offsetReg -> do
                        addInstrs $ ozLoad baseReg location
                                 <> ozSubOffset baseReg baseReg offsetReg
                        return $ Just baseReg
                    _ -> return Nothing

        Right (TypedValLvalue _ location offset _ _) ->
            if noOffset == offset then do
                -- Load the address directly
                register <- useRegister
                addInstrs $ ozLoadAddress register location
                return $ Just register
            else do
                -- Load the address and add offset
                baseReg <- useRegister
                offsetReg <- compileExpr offset
                case offsetReg of
                    Just offsetReg -> do
                        addInstrs $ ozLoadAddress baseReg location
                                 <> ozSubOffset baseReg baseReg offsetReg
                        return $ Just baseReg
                    _ -> return Nothing

loadSymbol :: TypedLvalue -> EitherState BlockState (Maybe Register)
loadSymbol (TypedValLvalue _ location offset _ _) = do
    register <- useRegister
    if noOffset == offset then do
        addInstrs $ ozLoad register location
        return $ Just register
    else do
        -- Compute a value offset
        baseReg <- useRegister
        offsetReg <- compileExpr offset
        case offsetReg of
            Just offsetReg -> do
                addInstrs $ ozLoadAddress baseReg location
                         <> ozSubOffset baseReg baseReg offsetReg
                         <> ozLoadIndirect register baseReg
                return $ Just register

            _ -> return Nothing

loadSymbol (TypedRefLvalue _ location offset _ _) =
    if noOffset == offset then do
        ptr <- useRegister
        register <- useRegister
        addInstrs $ ozLoad ptr location
                 <> ozLoadIndirect register ptr
        return $ Just register
    else do
        -- Compute a reference offset
        register <- useRegister
        baseReg <- useRegister
        offsetReg <- compileExpr offset
        case offsetReg of
            Just offsetReg -> do
                addInstrs $ ozLoad baseReg location
                         <> ozSubOffset baseReg baseReg offsetReg
                         <> ozLoadIndirect register baseReg
                return $ Just register
            _ -> return Nothing

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
    addErrorsOr (analyseLvalue (rootAliases $ blockRootSyms current) (blockLocalSyms current) lvalue)
                (storeSymbol register)

storeSymbol :: Register -> TypedLvalue -> EitherState BlockState ()
storeSymbol register (TypedValLvalue _ location offset _ _) =
    if noOffset == offset then
        addInstrs $ ozStore location register
    else do
        baseReg <- useRegister
        offsetReg <- compileExpr offset
        case offsetReg of
            Just offsetReg -> addInstrs $ ozLoadAddress baseReg location
                                       <> ozSubOffset baseReg baseReg offsetReg
                                       <> ozStoreIndirect baseReg register
            _ -> return ()

storeSymbol register (TypedRefLvalue _ location offset _ _) =
    if noOffset == offset then do
        ptr <- useRegister
        addInstrs $ ozLoad         ptr location
                <> ozStoreIndirect ptr register
    else do
        baseReg <- useRegister
        offsetReg <- compileExpr offset
        case offsetReg of
            Just offsetReg -> addInstrs $ ozLoad baseReg location
                                       <> ozSubOffset baseReg baseReg offsetReg
                                       <> ozStoreIndirect baseReg register
            _ -> return ()
