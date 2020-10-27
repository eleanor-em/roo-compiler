{-# LANGUAGE OverloadedStrings #-}

module RooCompile where

import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text)
import qualified Data.Text as T

import Common

import RooAnalyse
import RooAst
import SymTable
import Oz
import RooPrettyPrinter (prettyStatement, prettyExpr)
import qualified Data.Map.Strict as Map
import Control.Monad.State


-- | Global state for the compiler. Stores:
--   * the root-level symbol table
--   * the instructions generated so far
--   * the next unused register
--   * the next "stack slot" available for saving registers (uses the end of the register list)
--   * the next unused label index
--   * whether the context is a tail-call
--   * the epilogue for this block (used to ensure returns clean up the stack)
--   * the next unused lambda index
data BlockState = BlockState
    { blockSyms :: RootTable
    , blockInstrs :: [Text]
    , blockNextReg :: Int 
    , blockStackReg :: Int
    , nextLabel :: Int
    , isTailCall :: Bool
    , blockEpilogue :: [Text]
    , blockNextLambda :: Int }

-- | Creates an empty block state from the given symbol table.
initialBlockState :: RootTable -> BlockState
initialBlockState symbols = BlockState symbols [] 0 0 0 False [] 0

-- | Adds the "this is a tail call" flag to the state.
setTailCall :: EitherState BlockState ()
setTailCall = do
    current <- getEither
    putEither (current { isTailCall = True })

-- | Removes the "this is a tail call" flag from the state.
unsetTailCall :: EitherState BlockState ()
unsetTailCall = do
    current <- getEither
    putEither (current { isTailCall = False })

-- | Add instructions to the state with indentation.
addInstrs :: [Text] -> EitherState BlockState ()
addInstrs instrs = addInstrsRaw (map addIndent instrs)

-- | Add instructions to the state without indentation.
addInstrsRaw :: [Text] -> EitherState BlockState ()
addInstrsRaw instrs = do
    current <- getEither
    let prevInstrs = blockInstrs current
    putEither (current { blockInstrs = prevInstrs <> instrs})


-- | Creates a comment by pretty-printing the statement.
--   Doesn't work nicely for multi-line statements.
commentStatement :: Statement -> EitherState BlockState ()
commentStatement st = addInstrs $ makeComment $ prettyStatement 0 st


-- | Compiles the symbols in a given program, with an initial symbol table provided.
compileSymbols :: RootTable -> Program -> (RootTable, [AnalysisError], [Procedure])
compileSymbols initial (Program recs arrs procs) = (symbols, errs, procs')
    where
        -- Count the number of lambda functions in the initial table
        lambdaCount = Map.size $ Map.filterWithKey (\key _ -> "__lambda" `T.isInfixOf` key)
                                                   (rootProcs initial)

        procs' = procs <> concat (evalState (mapM compileLambdas procs) lambdaCount)

        (errs, symbols) = getAllSymbols (Program recs arrs procs') initial

-- | Compile the program with the given symbol table, previous errors, and procedure list.
compileWithSymbols :: RootTable -> [AnalysisError] -> [Procedure] -> ([AnalysisError], [Text])
compileWithSymbols symbols errs procs = do
    -- Compile all the procedures in our program.
    let (errs', result) = execEither (mapM_ compileProc procs)
                                     (initialBlockState symbols)

    let output = addHeader symbols (blockInstrs result)

    let allErrs = errs <> errs'

    (allErrs, separate output)
    where
        separate = map (<> "\n")
        addHeader symbols = ((["call proc_main", "halt"] <> generateVtable symbols) <>)

-- | Compiles a program fragment; that is, a program that may not have a main procedure.
compileProgramFragment :: Program -> ([AnalysisError], [Text])
compileProgramFragment program = compileWithSymbols symbols errs procs
    where
        (symbols, errs, procs) = compileSymbols mempty program

-- | Returns any syntax errors from a program, drawing symbols from a list of includes.
verifyProgram :: Program -> [Program] -> [AnalysisError]
verifyProgram program includes = do
    -- Extract symbols from the includes...
    let (symbols, _, _) = unzip3 $ map (compileSymbols mempty) includes
    -- ...then finish the symbols with the main file
    let (symbols', errs, procs) = compileSymbols (mconcat symbols) program
    fst $ compileWithSymbols symbols' errs procs

-----------------------------------
-- Main Compilation Functions  
-----------------------------------

-- | Compiles a program.
compileProgram :: Program -> ([AnalysisError], [Text])
compileProgram program = do
    let (symbols, errs, procs) = compileSymbols mempty program
    if not (hasMain symbols) then
        (errs <> [AnalysisError 0 0 "main procedure with no parameters missing"], [])
    else
        compileWithSymbols symbols errs procs

-- | Compiles a procedure definition, updating the state with the instructions and any errors.
compileProc :: Procedure -> EitherState BlockState ()
compileProc (Procedure _ (ProcHeader (Ident pos procName) _) retType _ statements) = do
    current <- getEither

    case lookupProc (blockSyms current) procName of
        Just (_, locals) -> do
            -- Check that non-void procedures always return a value
            unless (retType == VoidTypeName || any returnsValue statements)
                (addErrors $ warnPos pos
                "control may reach the end of the procedure without returning a value")

            let stackSize = localStackSize locals
            let prologue = if stackSize > 0 then ozPushStackFrame stackSize else []
            let epilogue = (if stackSize > 0 then ozPopStackFrame  stackSize else [])
                        <> ["return"]

            putEither (current { blockEpilogue = epilogue })
            -- Load arguments
            let argPrologue = mconcat $ zipWith ozStore (map symLocation (localParams locals))
                                                        (map Register [0..])

            -- Initialise local variables to 0
            let paramCount = length $ localParams locals

            let localPrologue = if stackSize - paramCount > 0 then
                    makeComment "init locals" <> ozIntConst (Register 0) 0
                                            <> concatMap ((`ozStore` Register 0) . StackSlot)
                                                        [paramCount..stackSize - 1]
                else
                    []

            addInstrsRaw $ ["\n" <> makeProcLabel procName <> ":"]
                        <> makeComment "prologue"
            addInstrs $ prologue <> makeComment "load args" <> argPrologue
            addInstrsRaw [makeProcTailLabel procName <> ":"]
            addInstrs localPrologue

            mapM_ (\st -> resetBlockRegs >> compileStatement locals st) statements

            addInstrs (makeComment "epilogue" <> epilogue)

        Nothing  -> error "internal error: missed a procedure somehow"

-- | Compiles a single statement, updating the state with the instructions and any errors.
compileStatement :: LocalTable -> Statement -> EitherState BlockState ()
-- | write expr;
compileStatement locals st@(SWrite expr) = do
    commentStatement st
    compileWrite locals expr

-- | writeln expr;
compileStatement locals st@(SWriteLn expr) = do
    commentStatement st

    compileWrite locals expr
    addInstrs (ozWriteString "\\n")

-- | lvalue <- expr;
compileStatement locals st@(SAssign lvalue expr) = do
    commentStatement st

    current <- getEither
    let symbols = blockSyms current

    let analysed = do
        TypedExpr ty' expr' <- analyseExpression symbols locals expr
        sym <- analyseLvalue symbols locals lvalue
        return (ty', expr', sym)

    addErrorsOr analysed $ \(ty', expr', sym) -> do
        let ty = lvalueType sym
        if ty /= ty' then
            addErrors $ errorPos (locate expr)
                                 ("cannot assign " <> backticks ty' <> " to " <> backticks ty)
        else
            if isPrimitive ty || isFunction ty then do
                register <- compileExpr locals (simplifyExpression expr')
                storeSymbol locals sym <?> register
            else case expr' of
                ELvalue lvalue' -> do
                    addErrorsOr (analyseLvalue symbols locals lvalue')
                                (\sym' -> copyContents locals sym sym' (sizeof ty'))
                _ -> error "internal error: somehow had primitive on rhs when expecting lvalue"

compileStatement locals st@(SRead lvalue) = do
    commentStatement st

    current <- getEither
    let symbols = blockSyms current
    -- reserve a register for reading the value
    _ <- useRegister

    addErrorsOr (analyseLvalue symbols locals lvalue) $ \sym -> do
        let ty = lvalueType sym

        case ty of
            TInt  -> addInstrs ozReadInt
            TBool -> addInstrs ozReadBool
            _     -> addErrors $ errorPos (lvaluePos sym) $
                "expecting `integer` or `boolean` on RHS of `read`, found `" <> backticks ty

        storeLvalue locals lvalue (Register 0)

compileStatement locals st@(SCall ident args) = do
    commentStatement st
    compileCall locals ident args
    pure ()

-- dealing with if statements  
compileStatement locals (SIf expr statements) = do 
    addInstrs $ makeComment ("if " <> prettyExpr (fromLocated expr) <> " then")

    current <- getEither
    let symbols = blockSyms current
    fiLabel <- getLabel 

    addErrorsOr (analyse symbols) $ \expr' -> do 
        case simplifyExpression expr' of
            EConst (LitBool val) -> addErrors $
                warnPos (locate expr)
                        ("`if` condition is always " <> tshowBool val)
            _ -> pure ()

        -- get the register where true/false is stored + the current state after compilation
        register <- compileExpr locals (simplifyExpression expr')
        addInstrs (ozBranchOnFalse (fromJust register) fiLabel) 
        mapM_ (\st -> resetBlockRegs >> compileStatement locals st) statements 
        addInstrs $ makeComment "fi"
        addInstrsRaw [fiLabel <> ":"]
    where
        analyse symbols = do 
            TypedExpr ty expr' <- analyseExpression symbols locals expr

            -- condition expression is incorrectly typed 
            if ty /= TBool then 
                Left $ errorPos (locate expr)
                                ("expecting `boolean`, found " <> backticks ty)
            else
                return expr'

-- dealing with if/Else statements  
compileStatement locals (SIfElse expr ifStatements elseStatements) = do 
    addInstrs $ makeComment ("if " <> prettyExpr (fromLocated expr) <> " then")

    current <- getEither
    let symbols = blockSyms current
    elseLabel <- getLabel 
    afterLabel <- getLabel

    addErrorsOr (analyse symbols) $ \expr' -> do 
        case simplifyExpression expr' of
            EConst (LitBool val) -> addErrors $
                warnPos (locate expr)
                        ("`if` condition is always " <> tshowBool val)
            _ -> pure ()

        -- get the register where true/false is stored + the current state after compilation
        register <- compileExpr locals (simplifyExpression expr')
        -- if condition is false -> go to else 
        addInstrs (ozBranchOnFalse (fromJust register) elseLabel) 
        -- otherwise do these statements 
        mapM_ (\st -> resetBlockRegs >> compileStatement locals st) ifStatements 
        -- now goto after the if block 
        addInstrs (ozBranch afterLabel)
        addInstrs $ makeComment "else"
        addInstrsRaw [elseLabel <> ":"]
        mapM_ (\st -> resetBlockRegs >> compileStatement locals st) elseStatements
        addInstrs $ makeComment "fi"
        addInstrsRaw [afterLabel <> ":"]
    where
        analyse symbols = do 
            TypedExpr ty expr' <- analyseExpression symbols locals expr

            -- condition expression is incorrectly typed 
            if ty /= TBool then 
                Left $ errorPos (locate expr)
                                ("expecting `boolean`, found " <> backticks ty)
            else
                return expr'

-- dealing with while statements  
compileStatement locals (SWhile expr statements) = do 
    addInstrs $ makeComment ("while " <> prettyExpr (fromLocated expr) <> " do")

    current <- getEither
    let symbols = blockSyms current
    beginLabel <- getLabel
    falseLabel <- getLabel 

    addErrorsOr (analyse symbols) $ \expr' -> do 
        case simplifyExpression expr' of
            EConst (LitBool val) -> addErrors $
                warnPos (locate expr)
                        ("`while` condition is always " <> tshowBool val)
            _ -> pure ()

        let conditionLvals = lvaluesOf (fromLocated expr)
        let nonModifiedLvals = filter (\lval -> not (any (modifiesLvalue lval) statements))
                                      conditionLvals

        when (length nonModifiedLvals == length conditionLvals)
             (addErrors $ warnPos
                (locate expr)
                "possible infinite loop: the condition does not change between iterations")

        addInstrsRaw [beginLabel <> ":"]
        -- get the register where true/false is stored + the current state after compilation
        register <- compileExpr locals (simplifyExpression expr')
        -- if condition is false --> skip the while loop 
        addInstrs (ozBranchOnFalse (fromJust register) falseLabel) 
        -- otherwise do these statements
        mapM_ (\st -> resetBlockRegs >> compileStatement locals st) statements 
        -- go to start 
        addInstrs (ozBranch beginLabel)
        addInstrs $ makeComment "od"
        addInstrsRaw [falseLabel <> ":"]
        
    where
        analyse symbols = do 
            TypedExpr ty expr' <- analyseExpression symbols locals expr

            -- condition expression is incorrectly typed 
            if ty /= TBool then 
                Left $ errorPos (locate expr)
                                ("expecting `boolean`, found " <> backticks ty)
            else
                return expr'

compileStatement locals st@(SReturn expr) = do
    commentStatement st

    current <- getEither
    let symbols = blockSyms current

    addErrorsOr (analyseExpression symbols locals expr) $ \(TypedExpr ty expr') -> do
        let ty' = localRetType locals

        if ty /= ty' then
            addErrors $ errorPos (locate expr)
                                 ("expecting " <> backticks ty' <> " on RHS of `return`, found "
                                               <> backticks ty)
        else do
            let compile = do
                register <- compileExpr locals (simplifyExpression expr')
                (addInstrs . ozMove ozReturnRegister) <?> register
            -- Tail call optimisation :-)
            case expr' of
                EFunc target _ ->
                    if fromIdent target == localProcName locals then do
                        setTailCall
                        compile
                        unsetTailCall
                    else
                        compile
                _ -> compile
            addInstrs $ blockEpilogue current

compileExpr :: LocalTable -> Expression -> EitherState BlockState (Maybe Register)
compileExpr locals (ELvalue lvalue) = loadLvalue locals lvalue

compileExpr _ (EConst lit) = Just <$> loadConst lit

compileExpr locals (EUnOp op expr) = do
    eval <- compileExpr locals (fromLocated expr)
    case eval of
        Just eval -> do
            result <- useRegister
            addInstrs (ozOp result eval)
            return $ Just result
        _ -> return Nothing

    where
        ozOp = case op of
            UnNot    -> ozNot
            UnNegate -> ozNeg

compileExpr locals (EBinOp op lexp rexp) = do
    lhs <- compileExpr locals (fromLocated lexp)
    rhs <- compileExpr locals (fromLocated rexp)
    case (lhs, rhs) of
        (Just lhs, Just rhs) -> do
            result <- useRegister
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

compileExpr locals (EFunc func args) = do
    current <- getEither
    let usedRegisters = blockNextReg current - 1

    when (usedRegisters >= 0) (do
        -- Save the current registers
        addInstrs $ makeComment "push registers"
        mapM_ (pushRegister . Register) [0..usedRegisters])

    addInstrs $ makeComment $ "call " <> fromIdent func
    result <- useRegister
    returned <- compileCall locals func args
    addInstrs $ ozMove result ozReturnRegister

    when (usedRegisters >= 0) (do
        -- Save the current registers
        addInstrs $ makeComment "pop registers"
        mapM_ (popRegister . Register) [0..usedRegisters])
    
    case returned of
        Just _   -> return $ Just result
        _        -> return Nothing

compileExpr locals (ELambda _ (LocatedTypeName pos _) _ _) = do
    -- Allocate a lambda
    current <- getEither
    let nextLambda = blockNextLambda current
    putEither ( current { blockNextLambda = nextLambda + 1 })

    addInstrs $ makeComment ("call lambda " <> lambdaLabel nextLambda)

    compileExpr locals (ELvalue (LId (Ident pos (lambdaLabel nextLambda))))


-- | Compile a `write` statement with a given expression, updating the state with the instructions
--   and any errors.
compileWrite :: LocalTable -> LocatedExpr -> EitherState BlockState ()
--   Special case to handle string literals.
compileWrite _ (LocatedExpr _ (EConst (LitString str)))
    = addInstrs (ozWriteString str)

compileWrite locals expr = do    
    current <- getEither
    let symbols = blockSyms current

    addErrorsOr (analyseExpression symbols locals expr) $ \(TypedExpr ty expr) -> do
        register <- compileExpr locals (simplifyExpression expr)

        let op TInt  = addInstrs . ozWriteInt
            op TBool = addInstrs . ozWriteBool
            op _     = error $ "internal error: attempted to write invalid type "
                            <> T.unpack (backticks ty)
            in op ty <?> register

-- | Compile a procedure call given the procedure's name and its actual arguments.
--   Updates the state with the instructions and any errors.
compileCall :: LocalTable -> Ident -> [LocatedExpr] -> EitherState BlockState (Maybe Register)
compileCall locals ident args = do
    current <- getEither
    let symbols = blockSyms current
    let result = do
        (typedArgs, params, retType) <- typecheckCall symbols locals ident args
        -- Compile arguments
        let typedArgs' = zip typedArgs params
        let currentReg = blockNextReg current
        let next = current { blockInstrs = [], blockNextReg = currentReg + length args }
        (registers, final) <- runEither (mapM compileArg typedArgs') next

        -- handle the case where it's a HOF
        case lookupProc (blockSyms current) (fromIdent ident) of
            Just _ ->
                if isTailCall current then do
                    let stores = concatMap (uncurry ozStore)
                                           (zip (map StackSlot [0..]) (catMaybes registers))

                    let instrs = stores <> ozBranch (makeProcTailLabel (fromIdent ident))
                    let nextLambda = blockNextLambda final
                    return (blockInstrs final <> map addIndent instrs, retType, nextLambda)

                else do
                    let moves = concatMap (uncurry ozMove)
                                          (zip (map Register [0..]) (catMaybes registers))

                    let instrs = moves <> ozCall (makeProcLabel (fromIdent ident))
                    let nextLambda = blockNextLambda final
                    return (blockInstrs final <> map addIndent instrs, retType, nextLambda)
            
            Nothing -> do
                (ptr, final') <- runEither (loadLvalue locals (LId ident)) final

                case ptr of 
                    Just ptr -> do
                        let moves = concatMap (uncurry ozMove)
                                              (zip (map Register [0..]) (catMaybes registers))
                        let instrs = moves <> ozSetVPtr ptr <> ozCall vTableLabel
                        let nextLambda = blockNextLambda final'
                        return (blockInstrs final' <> map addIndent instrs, retType, nextLambda)
                    Nothing -> error "internal error: did not catch all HOF call cases :("

    case result of
        Left errs    -> do
            addErrors errs
            return Nothing

        Right (instrs, retType, nextLambda) -> do
            addInstrsRaw instrs
            current' <- getEither
            putEither (current' { blockNextLambda = nextLambda })
            return $ case retType of
                TVoid -> Nothing
                _     -> Just ozReturnRegister
    where
        -- | Compile the argument, loading the address for refsand the value for vals
        compileArg (TypedExpr _ (ELvalue lvalue), RefSymbol _)
            = loadAddress locals lvalue
        compileArg (TypedExpr _ expr, _)
            = compileExpr locals (simplifyExpression expr)

-----------------------------------
-- Register Management 
-----------------------------------

loadLvalue :: LocalTable -> Lvalue -> EitherState BlockState (Maybe Register)
loadLvalue locals lvalue = do
    current <- getEither

    case analyseLvalue (blockSyms current) locals lvalue of
        Left errs -> case lvalue of
            LId (Ident _ name) ->
                case Map.lookup name (rootFuncPtrs $ blockSyms current) of
                    Just (procIndex, _) ->
                        pure <$> loadConst (LitInt procIndex)

                    Nothing -> do 
                        addErrors errs
                        return Nothing
            _ -> do
                addErrors errs
                return Nothing
        Right sym -> loadSymbol locals sym


loadAddress :: LocalTable -> Lvalue -> EitherState BlockState (Maybe Register)
loadAddress locals lvalue = do
    current <- getEither

    case analyseLvalue (blockSyms current) locals lvalue of
        Left errs -> do
            addErrors errs
            return Nothing

        Right lval -> do
            let offset = lvalueOffset lval
            let location = lvalueLocation lval

            if noOffset == offset then do
                -- Load the address directly
                register <- useRegister
                addInstrs $ loadOp lval register location
                return $ Just register
            else do
                -- Load the address and add offset
                baseReg <- useRegister
                offsetReg <- compileExpr locals offset
                case offsetReg of
                    Just offsetReg -> do
                        addInstrs $ loadOp lval baseReg location
                                 <> ozSubOffset baseReg baseReg offsetReg
                        return $ Just baseReg
                    _ -> return Nothing
    where
        loadOp lval = case lval of
            TypedValLvalue {} -> ozLoadAddress
            TypedRefLvalue {} -> ozLoad

loadSymbol :: LocalTable -> TypedLvalue -> EitherState BlockState (Maybe Register)
loadSymbol locals lval = do
    let offset = lvalueOffset lval
    let location = lvalueLocation lval

    if noOffset == offset then do
        register <- useRegister
        noOffsetOp register
        return $ Just register
    else do
        -- Compute a reference offset
        register <- useRegister
        baseReg <- useRegister
        offsetReg <- compileExpr locals offset
        case offsetReg of
            Just offsetReg -> do
                addInstrs $ offsetLoad baseReg location
                         <> ozSubOffset baseReg baseReg offsetReg
                         <> ozLoadIndirect register baseReg
                return $ Just register
            _ -> return Nothing
    where
        noOffsetOp register = case lval of
            TypedValLvalue {} ->
                addInstrs $ ozLoad register (lvalueLocation lval) 

            TypedRefLvalue {} -> do
                ptr <- useRegister
                addInstrs $ ozLoad ptr (lvalueLocation lval)
                         <> ozLoadIndirect register ptr
        
        offsetLoad = case lval of
            TypedValLvalue {} -> ozLoadAddress
            TypedRefLvalue {} -> ozLoad

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

-- handle offset needs to be handled per rval and lval 
loadSlot :: TypedLvalue -> Register -> EitherState BlockState Register 
loadSlot lval offsetReg = do
    register <- useRegister 
    baseReg <- useRegister 
    addInstrs $ offsetLoad baseReg (lvalueLocation lval) 
            <> ozSubOffset baseReg baseReg offsetReg
            <> ozLoadIndirect register baseReg 
    return register 

    where 
        offsetLoad = case lval of 
            TypedValLvalue {} -> ozLoadAddress 
            TypedRefLvalue {} -> ozLoad

storeContent :: TypedLvalue -> Register -> Register -> EitherState BlockState ()
storeContent lval offsetReg fromRegister = do
    baseReg <- useRegister 
    addInstrs $ offsetLoad baseReg (lvalueLocation lval) 
            <> ozSubOffset baseReg baseReg offsetReg
            <> ozStoreIndirect baseReg fromRegister 

    where 
        offsetLoad = case lval of 
            TypedValLvalue {} -> ozLoadAddress 
            TypedRefLvalue {} -> ozLoad

copyContents :: LocalTable -> TypedLvalue -> TypedLvalue -> Int -> EitherState BlockState () 
copyContents locals lval rval size = do
    -- Calculate the offsets here and then pass through the offsetReg with our own register 
    offsetReg' <- compileExpr locals (lvalueOffset lval)
    offsetReg'' <- compileExpr locals (lvalueOffset rval)
    case (offsetReg', offsetReg'') of 
        (Just offsetReg', Just offsetReg'') -> copyContentsRec lval rval offsetReg' offsetReg'' size 
        _ ->  pure $ ()
    
copyContentsRec :: TypedLvalue -> TypedLvalue -> Register -> Register -> Int -> EitherState BlockState ()
copyContentsRec _ _ _ _ 0 = pure $ ()
copyContentsRec lval rval lOffset rOffset slotsRemaining = do 

    -- load address from rval into a register 
    fromRegister <- loadSlot rval rOffset 
    
    -- using the address from before, store content into our lval 
    storeContent lval lOffset fromRegister
    incrReg <- loadConst (LitInt 1) 
    addInstrs $ ozPlus rOffset rOffset incrReg 
    addInstrs $ ozPlus lOffset lOffset incrReg
    copyContentsRec lval rval lOffset rOffset (slotsRemaining - 1)
    
storeLvalue :: LocalTable -> Lvalue -> Register -> EitherState BlockState ()
storeLvalue locals lvalue register = do
    current <- getEither
    addErrorsOr (analyseLvalue (blockSyms current) locals lvalue)
                (\sym -> storeSymbol locals sym register)

storeSymbol :: LocalTable -> TypedLvalue -> Register -> EitherState BlockState ()
storeSymbol locals lval register = do
    let offset = lvalueOffset lval

    if noOffset == offset then
        noOffsetOp
    else do
        baseReg <- useRegister
        offsetReg <- compileExpr locals offset
        case offsetReg of
            Just offsetReg -> addInstrs $ offsetLoad baseReg (lvalueLocation lval)
                                       <> ozSubOffset baseReg baseReg offsetReg
                                       <> ozStoreIndirect baseReg register
            _ -> return ()
    where
        noOffsetOp = case lval of
            TypedValLvalue {} ->
                addInstrs $ ozStore (lvalueLocation lval) register

            TypedRefLvalue {} -> do
                ptr <- useRegister
                addInstrs $ ozLoad ptr (lvalueLocation lval)
                         <> ozStoreIndirect ptr register
        
        offsetLoad = case lval of
            TypedValLvalue {} -> ozLoadAddress
            TypedRefLvalue {} -> ozLoad


-----------------------------------
-- Register Management
-----------------------------------

-- | Resets the allocation of registers in the state.
resetBlockRegs :: EitherState BlockState ()
resetBlockRegs = do
    current <- getEither
    putEither (current { blockNextReg = 0 })

-- | Allocates a new register, and returns the register.
useRegister :: EitherState BlockState Register
useRegister = do
    current <- getEither
    let register = blockNextReg current
    if register >= 1024 then
        error "internal error: ran out of registers"
    else do
        putEither (current { blockNextReg = register + 1})
        return $ Register register

-- | Pushes a register to the faux-stack formed by the final registers.
--   Used to save registers between procedure calls on the RHS of statements.
pushRegister :: Register -> EitherState BlockState ()
pushRegister register = do
    current <- getEither
    let head = blockStackReg current + 1
    putEither (current { blockStackReg = head })

    addInstrs $ ozMove (ozExtraRegisters head) register

-- | Pops a register from the faux-stack formed by the final registers.
--   Used to save registers between procedure calls on the RHS of statements.
popRegister :: Register -> EitherState BlockState ()
popRegister register = do
    current <- getEither
    let head = blockStackReg current
    putEither (current { blockStackReg = head - 1 })

    addInstrs $ ozMove register (ozExtraRegisters head)

-------

getLabel :: EitherState BlockState Text 
getLabel = do 
    current <- getEither 
    let currentLabel = nextLabel current 
    putEither (current { nextLabel = currentLabel + 1})
    return $ "label_" <> (tshow currentLabel)

-----------------------------------
-- Text processing for prettifying generated Oz code
-----------------------------------

addIndent :: Text -> Text
addIndent "" = ""
addIndent str
    | T.head str == '#' = str
    | otherwise         = "    " <> str

makeComment :: Text -> [Text]
makeComment str = ["# " <> T.replace "\n" "" str]

makeProcLabel :: Text -> Text
makeProcLabel = ("proc_" <>)

makeProcTailLabel :: Text -> Text
makeProcTailLabel name = "proc_" <> name <> "_loaded"

vTableLabel :: Text
vTableLabel = "__vtable"

lambdaLabel :: Int -> Text
lambdaLabel i = "__lambda" <> tshow i



-- | Searches a procedure for any lambda functions, turning them into procedure definitions.
compileLambdas :: Procedure -> State Int [Procedure]
compileLambdas (Procedure _ _ _ _ statements)
    = do
        procs <- mapM compileLambdasInner statements
        return $ concat procs

-- | Search the statement for any lambda functions, and turn them into procedure definitions.
--   The state encodes the current used index.
compileLambdasInner :: Statement -> State Int [Procedure]
compileLambdasInner statement
    = case statement of
        SAssign _ rhs -> compileExprLambdas rhs

        SCall _ args  -> concat <$> mapM compileExprLambdas args

        SIf expr body -> do
            exprs <- compileExprLambdas expr
            inners <- concat <$> mapM compileLambdasInner body
            return $ exprs <> inners

        SIfElse expr bodyIf bodyElse -> do
            exprs <- compileExprLambdas expr
            innersIf <- concat <$> mapM compileLambdasInner bodyIf
            innersElse <- concat <$> mapM compileLambdasInner bodyElse
            return $ exprs <> innersIf <> innersElse

        SWhile expr body -> do
            exprs <- compileExprLambdas expr
            inners <- concat <$> mapM compileLambdasInner body
            return $ exprs <> inners

        SReturn expr -> compileExprLambdas expr

        _ -> pure []

-- | Compiles a Lambda Expression 
compileExprLambdas :: LocatedExpr -> State Int [Procedure]
compileExprLambdas (LocatedExpr _ (EBinOp _ lhs rhs)) = do
    ls <- compileExprLambdas lhs
    rs <- compileExprLambdas rhs
    return $ ls <> rs
compileExprLambdas (LocatedExpr _ (EUnOp _ inner))
    = compileExprLambdas inner
compileExprLambdas (LocatedExpr _ (EFunc _ args))
    = concat <$> mapM compileExprLambdas args
compileExprLambdas (LocatedExpr pos (ELambda params retType varDecls body)) = do
    let (LocatedTypeName _ retTypeInner) = retType

    current <- get
    put (current + 1)
    let header = ProcHeader (Ident pos (lambdaLabel current)) params

    return $ pure $ Procedure pos header retTypeInner varDecls body


compileExprLambdas _ = pure []

-- | Generates the vtable instructions for the given symbol table. This works by assigning an index
--   to every procedure, and performing a linear scan on a dedicated register to search for the
--   appropriate procedure.
generateVtable :: RootTable -> [Text]
generateVtable table = mconcat
    [ [ vTableLabel <> ":" ]
    , mconcat branches
    , map addIndent (ozBranch "__vtable_end")
    , mconcat labels
    , [ "__vtable_end:" ]
    , map addIndent $ mconcat
        [ ozWriteString "invalid virtual pointer: "
        , ozReadVPtr (Register 0)
        , ozWriteInt (Register 0)
        , ozWriteString "\\n(exiting)"
        , [ "halt" ] ] ]
    where
        (branches, labels) = unzip $ map extractVtableData (Map.toAscList (rootVtable table))
        extractVtableData (index, (_, locals)) = (branch, target)
            where
                procName = localProcName locals
                label  = "__vptr_" <> procName
                branch = map addIndent (ozCmpBranchVPtr index label)
                target = mconcat
                    [ [ label <> ":" ]
                    , map addIndent (ozCall (makeProcLabel procName))
                    , [ addIndent "return" ] ]