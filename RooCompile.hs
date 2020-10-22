{-# LANGUAGE OverloadedStrings #-}

module RooCompile where

import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Common

import RooAnalyse
import RooAst
import SymTable
import Oz
import Control.Monad (unless)
import RooPrettyPrinter (prettyStatement)

-- | effectively now the global state 
data BlockState = BlockState
    { blockSyms :: RootTable
    , blockInstrs :: [Text]
    , blockNextReg :: Int 
    , nextLabel :: Int}

initialBlockState :: RootTable -> BlockState
initialBlockState symbols = BlockState symbols [] 0 0

resetBlockRegs :: EitherState BlockState ()
resetBlockRegs = do
    current <- getEither
    putEither (current { blockNextReg = 0 })

useRegister :: EitherState BlockState Register
useRegister = do
    current <- getEither
    let register = blockNextReg current
    putEither (current { blockNextReg = register + 1})
    return $ Register register

-- | Add instructions to the state with indentation.
addInstrs :: [Text] -> EitherState BlockState ()
addInstrs instrs = addInstrsRaw (map addIndent instrs)

addInstrsMaybe :: Maybe [Text] -> EitherState BlockState ()
addInstrsMaybe (Just instrs) = addInstrs instrs
addInstrsMaybe _ = pure ()

-- | Add instructions to the state without indentation.
addInstrsRaw :: [Text] -> EitherState BlockState ()
addInstrsRaw instrs = do
    current <- getEither
    let prevInstrs = blockInstrs current
    putEither (current { blockInstrs = prevInstrs <> instrs})

compileProgram :: Program -> Either [AnalysisError] [Text]
compileProgram program@(Program _ _ procs) = do
    let (errs, symbols) = getAllSymbols program

    if not (hasMain symbols) then
        Left (errs <> [AnalysisError 0 0 "main procedure with no parameters missing"])
    else do
        -- Compile all the procedures in our program. 

        -- execEither: "run the state, return errors and final state"
        --  runEither: "run the state, return errors, final state, *and* an extra value"
        let (errs', result) = execEither (mapM_ compileProc procs) (initialBlockState symbols)
        let output = addHeader (blockInstrs result)

        let allErrs = errs <> errs'

        if null allErrs then
            Right (separate output)
        else
            Left allErrs
    where
        separate  = map (<> "\n")
        addHeader = (["call proc_main", "halt"] <>)

compileProc :: Procedure -> EitherState BlockState ()
compileProc (Procedure _ (ProcHeader (Ident _ procName) _) _ statements) = do
    current <- getEither

    case lookupProc (blockSyms current) procName of
        Just (_, locals) -> do
            let stackSize = localStackSize locals
            let prologue = if stackSize > 0 then ozPushStackFrame stackSize else []
            let epilogue = if stackSize > 0 then ozPopStackFrame  stackSize else []

            -- Load arguments
            let argPrologue = mconcat $ zipWith ozStore (map symLocation (localParams locals))
                                                        (map Register [0..])

            addInstrsRaw $ ["\n" <> makeProcLabel procName <> ":"]
                        <> addComment "prologue"
            addInstrs (prologue <> argPrologue)

            -- TODO: initialise locals to 0
            
            mapM_ (\st -> resetBlockRegs >> compileStatement locals st) statements

            addInstrs (addComment "epilogue" <> epilogue <> ["return"])

        Nothing  -> error "internal error: missed a procedure somehow"

commentStatement :: Statement -> EitherState BlockState ()
commentStatement (SIf _ _) = error "commentStatement: cannot handle `if`"
commentStatement (SWhile _ _) = error "commentStatement: cannot handle `while`"
commentStatement SIfElse {} = error "commentStatement: cannot handle `if...else`"
commentStatement st = addInstrs $ addComment $ prettyStatement 0 st

compileStatement :: LocalTable -> Statement -> EitherState BlockState ()
-- | write str;
--   Special case to handle string literals.
compileStatement _ st@(SWrite (LocatedExpr _ (EConst (LitString str))))
    = do
        commentStatement st
        addInstrs (ozWriteString str)

-- | write expr;
compileStatement locals st@(SWrite expr) = do
    commentStatement st
    
    current <- getEither
    let symbols = rootAliases (blockSyms current)

    addErrorsOr (analyseExpression symbols locals expr) $ \(TypedExpr ty expr) -> do
        register <- compileExpr locals expr

        let op TInt  = addInstrs . ozWriteInt
            op TBool = addInstrs . ozWriteBool
            op _     = error $ "internal error: attempted to write invalid type `" <> show ty <> "`"
            in op ty <?> register

-- | writeln expr;
compileStatement locals st@(SWriteLn expr) = do
    commentStatement st

    compileStatement locals (SWrite expr)
    addInstrs (ozWriteString "\\n")

-- | lvalue <- expr;
compileStatement locals st@(SAssign lvalue expr) = do
    commentStatement st

    current <- getEither
    let symbols = blockSyms current

    addErrorsOr (analyse symbols) $ \(ty', expr', sym) -> do
        let ty = lvalueType sym

        case ty of
            TArray _ _ -> typeError ty
            TRecord _  -> typeError ty
            _ -> do    
                if ty /= ty' then
                    let err  = "expecting `" <> tshow ty' <> "` on RHS of `<-`, found `" <> tshow ty <> "`"
                        note = "`" <> lvalueName sym <> "` declared here:" in
                    addErrors $ errorWithNote (locate expr) err (lvaluePos sym) note
                else do
                    register <- compileExpr locals expr'
                    storeSymbol locals sym <?> register
    where
        analyse symbols = do
            TypedExpr ty' expr' <- analyseExpression (rootAliases symbols) locals expr
            sym <- analyseLvalue (rootAliases symbols) locals lvalue
            return (ty', expr', sym)
        typeError ty = includeEither $ liftOne $ errorPos (locateLvalue lvalue) $
            "expected variable of primitive type on LHS of `<-`, found `" <> tshow ty <> "`"

compileStatement locals st@(SRead lvalue) = do
    commentStatement st

    current <- getEither
    let symbols = blockSyms current
    -- reserve a register for reading the value
    _ <- useRegister

    addErrorsOr (analyseLvalue (rootAliases symbols) locals lvalue) $ \sym -> do
        let ty = lvalueType sym

        case ty of
            TInt  -> addInstrs ozReadInt
            TBool -> addInstrs ozReadBool
            _     -> do
                -- TODO: error with note
                let err  = "expecting `integer` or `boolean` after `read`, found `" <> tshow ty <> "`" in
                    includeEither $ liftOne $ errorPos (lvaluePos sym) err

        storeLvalue locals lvalue (Register 0)

compileStatement locals st@(SCall (Ident pos procName) args) = do
    commentStatement st

    current <- getEither
    let symbols = blockSyms current

    case result current symbols of
        Left errs    -> addErrors errs
        Right instrs -> addInstrsRaw instrs
    
    where
        -- | Checks whether the expression and symbol are either:
        --   lvalue + ref
        --   any + val
        checkRefArgs (TypedExpr _ (ELvalue _)) (ProcSymbol (RefSymbol _) _ _ _) = True
        checkRefArgs _ (ProcSymbol (RefSymbol _) _ _ _) = False
        checkRefArgs _ _ = True

        -- | Compile the argument, loading the address for ref types and the value for val types
        compileArg (TypedExpr _ (ELvalue lvalue), ProcSymbol (RefSymbol _) _ _ _)
            = loadAddress locals lvalue
        compileArg (TypedExpr _ expr, _)
            = compileExpr locals expr

        result current symbols = do
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
            let next = current { blockInstrs = [], blockNextReg = length args }
            (registers, final) <- runEither (mapM compileArg typedArgs') next

            let moves = concatMap (uncurry ozMove)
                                (zip (map Register [0..]) (catMaybes registers))

            return $ blockInstrs final <>
                    map addIndent (moves <> ozCall (makeProcLabel procName))

compileStatement _ _ = error "compileStatement: not yet implemented"

compileExpr :: LocalTable -> Expression -> EitherState BlockState (Maybe Register)
compileExpr locals (ELvalue lvalue) = loadLvalue locals lvalue

compileExpr _ (EConst lit) = Just <$> loadConst lit

compileExpr locals (EUnOp op expr) = do
    result <- useRegister
    eval <- compileExpr locals (fromLocated expr)
    case eval of
        Just eval -> do
            addInstrs (ozOp result eval)
            return $ Just result
        _ -> return Nothing        

    where
        ozOp = case op of
            UnNot    -> ozNot
            UnNegate -> ozNeg

compileExpr locals (EBinOp op lexp rexp) = do
    result <- useRegister
    lhs <- compileExpr locals (fromLocated lexp)
    rhs <- compileExpr locals (fromLocated rexp)
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

loadLvalue :: LocalTable -> Lvalue -> EitherState BlockState (Maybe Register)
loadLvalue locals lvalue = do
    current <- getEither
    case analyseLvalue (rootAliases $ blockSyms current) locals lvalue of
        Left errs -> do
            addErrors errs
            return Nothing
        Right sym -> loadSymbol locals sym


loadAddress :: LocalTable -> Lvalue -> EitherState BlockState (Maybe Register)
loadAddress locals lvalue = do
    current <- getEither
    case analyseLvalue (rootAliases $ blockSyms current) locals lvalue of
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
                offsetReg <- compileExpr locals offset
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
                offsetReg <- compileExpr locals offset
                case offsetReg of
                    Just offsetReg -> do
                        addInstrs $ ozLoadAddress baseReg location
                                 <> ozSubOffset baseReg baseReg offsetReg
                        return $ Just baseReg
                    _ -> return Nothing

loadSymbol :: LocalTable -> TypedLvalue -> EitherState BlockState (Maybe Register)
loadSymbol locals (TypedValLvalue _ location offset _ _) = do
    register <- useRegister
    if noOffset == offset then do
        addInstrs $ ozLoad register location
        return $ Just register
    else do
        -- Compute a value offset
        baseReg <- useRegister
        offsetReg <- compileExpr locals offset
        case offsetReg of
            Just offsetReg -> do
                addInstrs $ ozLoadAddress baseReg location
                         <> ozSubOffset baseReg baseReg offsetReg
                         <> ozLoadIndirect register baseReg
                return $ Just register

            _ -> return Nothing

loadSymbol locals (TypedRefLvalue _ location offset _ _) =
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
        offsetReg <- compileExpr locals offset
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

storeLvalue :: LocalTable -> Lvalue -> Register -> EitherState BlockState ()
storeLvalue locals lvalue register = do
    current <- getEither
    addErrorsOr (analyseLvalue (rootAliases $ blockSyms current) locals lvalue)
                (\sym -> storeSymbol locals sym register)

storeSymbol :: LocalTable -> TypedLvalue -> Register -> EitherState BlockState ()
storeSymbol locals (TypedValLvalue _ location offset _ _) register =
    if noOffset == offset then
        addInstrs $ ozStore location register
    else do
        baseReg <- useRegister
        offsetReg <- compileExpr locals offset
        case offsetReg of
            Just offsetReg -> addInstrs $ ozLoadAddress baseReg location
                                       <> ozSubOffset baseReg baseReg offsetReg
                                       <> ozStoreIndirect baseReg register
            _ -> return ()

storeSymbol locals (TypedRefLvalue _ location offset _ _) register =
    if noOffset == offset then do
        ptr <- useRegister
        addInstrs $ ozLoad         ptr location
                <> ozStoreIndirect ptr register
    else do
        baseReg <- useRegister
        offsetReg <- compileExpr locals offset
        case offsetReg of
            Just offsetReg -> addInstrs $ ozLoad baseReg location
                                       <> ozSubOffset baseReg baseReg offsetReg
                                       <> ozStoreIndirect baseReg register
            _ -> return ()

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